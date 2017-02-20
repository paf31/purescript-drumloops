module Loops
  ( Passage
  , beat
  , silence
  , merge
  , Merge
  , Millis
  , Event
  , Track
  , Warp
  , swing
  , warp
  , Dur (..)
  , loop
  , Audio
  , Playable
  , track
  , play
  , playWith
  , degrade
  ) where

import Prelude
import Data.List.Lazy as Lazy
import Audio.Howler (Howl)
import Audio.Howler (HOWLER, play) as H
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setInterval, setTimeout)
import Data.Foldable (for_)
import Data.Int (round)
import Data.Int (toNumber) as Int
import Data.List (List(..), (:), singleton)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (wrap, class Newtype, over, over2)
import Data.Rational (Rational, (%), toNumber, fromInt)
import Math (sin, pi)

-- | A `Passage` is a finite list of timed samples, with a duration.
-- | Durations in a Track are absolute; in a Passage, they are relative.
-- |
-- | `Passage` is a `Semigroup` and a `Monoid` so it is simple to build complex Passages by
-- | concatenating simpler ones:
-- |
-- | ```purescript
-- | bd <> sn :: Passage
-- | ```
-- |
-- | Invariant: samples are in increasing time order.
newtype Passage s = Passage
  { length :: Rational
  , values :: List { sample :: s, offset :: Rational }
  }

type PassageUnsized s = List { sample :: s, offset :: Rational }

-- | Join two `Passage`s to play concurrently as one.
merge :: forall s. Passage s -> Passage s -> Passage s
merge = \(Passage a) (Passage b) -> Passage
    { length: max a.length b.length
    , values: go a.values b.values
    }
  where
    go :: PassageUnsized s -> PassageUnsized s -> PassageUnsized s
    go (Nil) x = x
    go x (Nil) = x
    go a@({sample: sa, offset: oa} : moreAs)
               b@({ sample: sb, offset: ob } : moreBs)
      | oa < ob =   { sample: sa, offset: oa } : go moreAs b
      | otherwise = { sample: sb, offset: ob } : go a moreBs

instance semigroupPassage :: Semigroup (Passage s) where
  append (Passage l1) (Passage l2) = Passage
    { length: l1.length + l2.length
    , values: l1.values <> map (\{ sample, offset } ->
                                 { sample
                                 , offset: l1.length + offset
                                 }) l2.values
    }

instance monoidPassage :: Monoid (Passage s) where
  mempty = Passage { length: 0 % 1, values: mempty }

-- | A `newtype` around `Passage` with a `Monoid` instance
-- | which merges passages (concurrently).
newtype Merge s = Merge (Passage s)

derive instance newtypeMerge :: Newtype (Merge s) _

-- The `merge` function for Passages,
-- though it plays the role of append in the `Passage` semigroup,
-- is distinct from append for `Passage`
instance semigroupMerge :: Semigroup (Merge s) where
  append = over2 Merge merge

instance monoidMerge :: Monoid (Merge s) where
  mempty = wrap mempty

-- | A unit-length passage consisting of a single sample.
beat :: forall s. s -> Passage s
beat s = Passage { length : 1 % 1
                 , values : singleton
                     { offset : 0 % 1
                     , sample : s
                     }
                 }

-- | A unit-length Passage with no sounds.
silence :: forall s. Passage s
silence = Passage { length: 1 % 1
                  , values: mempty
                  }

type Millis = Int

type Event s = { sample :: s, time :: Millis }

-- | A `Track` is an infinite list of samples.
-- |
-- | A `Track` can be created by using the `loop` function, or played
-- | using the `play` function.
newtype Track s = Track (Lazy.List (Event s))

derive instance newtypeTrack :: Newtype (Track s) _

-- | A monotonically increasing function of time.
-- |
-- | See `warp`.
newtype Warp = Warp (Number -> Number)

derive instance newtypeWarp :: Newtype Warp _

swing :: Number -> Warp
swing strength = Warp (\x -> x + sin (0.5 * pi * x) / strength)

-- | A generalization of swing.
-- | To preserve order, requires a monotonic increasing function of numbers.
-- | And unless the function intersects the 45 degree line infinitely often,
-- | this will change the speed of the input Track.
warp :: forall s. Warp -> Track s -> Track s
warp (Warp f) = over Track $ map \{sample,time} -> {sample, time: f' time}
  where f' t = round $ x * 1000.0
          where x = f $ Int.toNumber t / 1000.0

data Dur
  = Dur Rational -- cycle duration, in seconds
  | BPM Rational -- frequency, in cycles per minute

durMs :: Dur -> Rational
durMs (Dur x) = x * fromInt 1000
durMs (BPM x) = fromInt 60000 / x

-- | Create a `Track` which plays the given `Passage` infinitely, at the specified rate:
-- |
-- | ```purescript
-- | Passage (BPM 120) (bd <> sn) :: Track
-- | ```
-- |
-- | A Dur of 1 or a BPM of 60 causes each unit length in the Passage to render as one second.
loop :: forall s. Dur -> Passage s -> Track s
loop dur (Passage { length, values } ) = Track do
  n <- Lazy.iterate (_ + 1) 0
  Lazy.fromFoldable $ map (\ { sample, offset } ->
      { sample
      , time: round $ toNumber $ durMs dur
          * (fromInt n * length + offset) -- ! max: 596 hr
      })
    values

-- | The effect monad used by the `track` function.
type Audio e =
  Eff ( howler :: H.HOWLER
      , ref    :: REF
      , timer  :: TIMER
      | e
      )

-- | A process which enqueues a collection of samples for playback.
newtype Playable s e = Playable ((s -> Audio e Unit) -> Audio e Unit)

-- | Make a `Track` into a `Playable` by enqueing all of its (infinitely-many!)
-- | samples incrementally.
track :: forall e s. Track s -> Playable s e
track (Track xs) = Playable $ \bark -> do
  tRef <- newRef 0
  xsRef <- newRef xs
  void $ setInterval 1000 do -- every 1000 ms do this
    t' <- readRef tRef
    xs' <- readRef xsRef
    let cutoff = (t' + 1) * 1000 -- look this far into the future
    case Lazy.span (\x -> x.time < cutoff) xs' of
      { init, rest } -> do  -- = the next second and what follows that
        writeRef tRef (t' + 1)
        writeRef xsRef rest
        for_ init \{ sample, time } ->
          setTimeout (time - t' * 1000) (bark sample)

-- | Play a `Track`.
-- |
-- | This function returns an action which can be used to stop playback.
play :: forall e. Playable Howl e -> Audio e Unit
play = playWith id

playWith :: forall e s. (s -> Howl) -> Playable s e -> Audio e Unit
playWith g (Playable f) = f (H.play <<< g)

-- | "Degrade" a `Playable` process by ignoring samples with a certain
-- | probability.
degrade
  :: forall e s
   . Number
  -> Playable s ( random :: RANDOM | e )
  -> Playable s ( random :: RANDOM | e )
degrade prob (Playable f) = Playable $ \play' -> f $ \sample -> do
  x <- random
  when (x < prob) $ play' sample
