module Loops
  ( Sample (..)
  , Passage
  , merge
  , Merge
  , silence
  , bd
  , sn
  , hh
  , Millis
  , Event
  , Track
  , unsafeWarp
  , Dur (..)
  , loop
  , Audio
  , Playable
  , track
  , play
  , degrade
  ) where

import Prelude
import Audio.Howler (HOWLER, defaultProps, new, play) as H
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Random (RANDOM, random)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setInterval, setTimeout)
import Data.Foldable (for_)
import Data.Int (round)
import Data.Int (toNumber) as Int
import Data.List.Lazy as Lazy
import Data.List (List(..), (:), singleton)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (wrap, class Newtype, over, over2)
import Data.Rational (Rational, (%), toNumber, fromInt)


data Sample = Bd | Sn | Hh

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
newtype Passage = Passage
  { length :: Rational
  , values :: List { sample :: Sample, offset :: Rational }
  }

type PassageUnsized = List { sample :: Sample, offset :: Rational }   

-- | Join two Passages to play concurrently as one.
merge :: Passage -> Passage -> Passage
merge (Passage a) (Passage b) = Passage
    { length: max a.length b.length
    , values: sortyZip a.values b.values
    }
  where
    sortyZip :: PassageUnsized -> PassageUnsized -> PassageUnsized
      -- If its inputs are sorted, sortyZip's output is sorted too.
    sortyZip (Nil) x = x
    sortyZip x (Nil) = x
    sortyZip a@({sample: sa, offset: oa} : moreAs)
               b@({sample: sb, offset: ob} : moreBs)
      | oa < ob =   {sample: sa, offset: oa} : sortyZip moreAs b
      | otherwise = {sample: sb, offset: ob} : sortyZip a moreBs

instance semigroupPassage :: Semigroup Passage where
  append (Passage l1) (Passage l2) = Passage
    { length: l1.length + l2.length
    , values: l1.values <> map (\ { sample, offset } ->
                                 { sample
                                 , offset: l1.length + offset
                                 }) l2.values
    }

instance monoidPassage :: Monoid Passage where
  mempty = Passage { length: 0%1, values: mempty }

newtype Merge = Merge Passage

derive instance newtypeMerge :: Newtype Merge _

instance semigroupMerge :: Semigroup Merge where
  append = over2 Merge merge -- ! Pitfall: merge v. append.
    -- The merge function for Passages,
    -- though it plays the role of append in the Passage semigroup,
    -- is distinct from append for Passages

instance monoidMerge :: Monoid Merge where
  mempty = wrap mempty

beat :: Sample -> Passage
beat s = Passage { length : 1%1
                 , values : singleton
                     { offset : 0%1
                     , sample : s
                     }
                 }

-- | A unit-length Passage consisting of a single bass drum sample.
bd :: Passage
bd = beat Bd

-- | A unit-length Passage consisting of a single snare drum sample.
sn :: Passage
sn = beat Sn

-- | A unit-length Passage consisting of a single high hat sample.
hh :: Passage
hh = beat Hh

-- | A unit-length Passage with no sounds.
silence :: Passage
silence = Passage { length: 1%1
                  , values: mempty
                  }

type Millis = Int

type Event = { sample :: Sample, time :: Millis }

-- | A `Track` is an infinite list of samples.
-- |
-- | A `Track` can be created by using the `loop` function, or played
-- | using the `play` function.
newtype Track = Track (Lazy.List Event)

derive instance newtypeTrack :: Newtype Track _

-- | A generalization of swing.
-- | To preserve order, requires a monotonic increasing function of numbers.
-- | And unless the function intersects the 45 degree line infinitely often,
-- | this will change the speed of the input Track.
unsafeWarp :: (Number -> Number) -> Track -> Track
unsafeWarp f = over Track $ map \{sample,time} -> {sample, time: f' time}
  where f' t = round $ x * 1000.0
          where x = f $ Int.toNumber t / 1000.0

data Dur = Dur Rational -- cycle duration, in seconds
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
loop :: Dur -> Passage -> Track  -- ! assumes samples are ordered
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
      | e )

newtype Playable e = Playable ((Sample -> Audio e Unit) -> Audio e Unit)
  -- is like Track

track :: forall e. Track -> Playable e
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
play :: forall e. Playable e -> Audio e Unit
play (Playable f) = do 
  bdHowl <- H.new (H.defaultProps { urls = ["/wav/bd.wav"], volume = 1.0 })
  snHowl <- H.new (H.defaultProps { urls = ["/wav/sn.wav"], volume = 0.75 })
  hhHowl <- H.new (H.defaultProps { urls = ["/wav/hh.wav"], volume = 1.0 })
  let toHowl Bd = bdHowl
      toHowl Sn = snHowl
      toHowl Hh = hhHowl
  f $ toHowl >>> H.play

degrade :: forall e. Number
        -> Playable ( random :: RANDOM | e )
        -> Playable ( random :: RANDOM | e )
degrade prob (Playable f) = Playable $ \play' -> f $ \sample -> do
  x <-random
  when (x < prob) $ play' sample
