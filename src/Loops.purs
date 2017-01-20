module Loops
  ( Loop
  , bd
  , sn
  , hh
  , silence
  , BPM
  , Audio
  , Track
  , loop
  , track
  ) where

import Prelude
import Data.List.Lazy as Lazy
import Audio.Howler (HOWLER, defaultProps, new, play)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, clearInterval, setInterval, setTimeout)
import Data.Foldable (for_)
import Data.List (List, singleton)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)

data Sample = Bd | Sn | Hh

-- | A `Loop` is a finite list of timed samples, with a duration.
-- |
-- | `Loop` is a `Semigroup` and a `Monoid` so it is simple to build loops by
-- | concatenating simpler loops:
-- |
-- | ```purescript
-- | bd <> sn :: Loop
-- | ```
-- |
-- | A `Loop` can be turned into a `Track` using the `loop` function.
newtype Loop = Loop
  { length :: Int
  , values :: List { sample :: Sample, offset :: Int }
  }

instance semigroupLoop :: Semigroup Loop where
  append (Loop l1) (Loop l2) = Loop
    { length: l1.length + l2.length
    , values: l1.values <> map (\{ sample, offset } ->
                                 { sample
                                 , offset: l1.length + offset
                                 }) l2.values
    }

instance monoidLoop :: Monoid Loop where
  mempty = Loop { length: 0, values: mempty }

beat :: Sample -> Loop
beat s =
  Loop { length: 1
       , values: singleton
           { offset: 0
           , sample: s
           }
       }

-- | A loop which consists of a single bass drum sample.
bd :: Loop
bd = beat Bd

-- | A loop which consists of a single snare sample.
sn :: Loop
sn = beat Sn

-- | A loop which consists of a single high hat sample.
hh :: Loop
hh = beat Hh

-- | A loop which is silent for one beat.
silence :: Loop
silence =
  Loop { length: 1
       , values: mempty
       }

type Millis = Int

type Event = { sample :: Sample, time :: Millis }

-- | A `Track` is an infinite list of samples.
-- |
-- | A `Track` can be created by using the `loop` function, or played
-- | using the `track` function.
newtype Track = Track (Lazy.List Event)

-- | Beats per minute.
-- |
-- | Use `Data.Newtype.wrap` to create a value:
-- |
-- | ```purescript
-- | wrap 120 :: BPM
-- | ```
newtype BPM = BPM Int

derive instance newtypeBPM :: Newtype BPM _

-- | Create a `Track` which plays the given `Loop` infinitely, at the specified rate:
-- |
-- | ```purescript
-- | loop (wrap 120) (bd <> sn) :: Track
-- | ```
loop :: BPM -> Loop -> Track
loop bpm (Loop { length, values } ) = Track do
  let millis = 36000 / unwrap bpm
  n <- Lazy.iterate (_ + 1) 0
  Lazy.fromFoldable (map (\{ sample, offset } ->
                           { sample
                           , time: millis * (n * length + offset)
                           }) values)

-- | The effect monad used by the `track` function.
type Audio e =
  Eff ( howler :: HOWLER
      , ref    :: REF
      , timer  :: TIMER
      | e )

-- | Play a `Track`.
-- |
-- | This function returns an action which can be used to stop playback.
track
  :: forall e
   . Track
  -> Audio e (Audio e Unit)
track (Track xs) = do
  bdHowl <- new (defaultProps { urls = ["/wav/bd.wav"], volume = 1.0 })
  snHowl <- new (defaultProps { urls = ["/wav/sn.wav"], volume = 0.75 })
  hhHowl <- new (defaultProps { urls = ["/wav/hh.wav"], volume = 0.25 })
  let toHowl Bd = bdHowl
      toHowl Sn = snHowl
      toHowl Hh = hhHowl
  tRef <- newRef 0
  xsRef <- newRef xs
  clearInterval <$> setInterval 1000 do
    t' <- readRef tRef
    xs' <- readRef xsRef
    let cutoff = (t' + 1) * 1000
    case Lazy.span (\x -> x.time < cutoff) xs' of
      { init, rest } -> do
        writeRef tRef (t' + 1)
        writeRef xsRef rest
        for_ init \{ sample, time } ->
          setTimeout (time - t' * 1000) (play (toHowl sample))
