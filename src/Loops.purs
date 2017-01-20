module Loops
  ( Loop
  , bd
  , sn
  , hh
  , silence
  , BPM
  , Track
  , loop
  , track
  ) where

import Prelude
import Data.List.Lazy as Lazy
import Audio.Howler (HOWLER, defaultProps, new, play)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setInterval, setTimeout)
import Data.Foldable (for_)
import Data.List (List, singleton)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, unwrap)

data Sample = Bd | Sn | Hh

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

bd :: Loop
bd = beat Bd

sn :: Loop
sn = beat Sn

hh :: Loop
hh = beat Hh

silence :: Loop
silence =
  Loop { length: 1
       , values: mempty
       }

type Millis = Int

type Event = { sample :: Sample, time :: Millis }

newtype Track = Track (Lazy.List Event)

newtype BPM = BPM Int

derive instance newtypeBPM :: Newtype BPM _

loop :: BPM -> Loop -> Track
loop bpm (Loop { length, values } ) = Track do
  let millis = 36000 / unwrap bpm
  n <- Lazy.iterate (_ + 1) 0
  Lazy.fromFoldable (map (\{ sample, offset } ->
                           { sample
                           , time: millis * (n * length + offset)
                           }) values)

track
  :: forall e
   . Track
  -> Eff ( howler :: HOWLER
         , ref    :: REF
         , timer  :: TIMER
         | e
         ) Unit
track (Track xs) = void do
  bdHowl <- new (defaultProps { urls = ["/wav/bd.wav"], volume = 1.0 })
  snHowl <- new (defaultProps { urls = ["/wav/sn.wav"], volume = 0.75 })
  hhHowl <- new (defaultProps { urls = ["/wav/hh.wav"], volume = 0.25 })
  let toHowl Bd = bdHowl
      toHowl Sn = snHowl
      toHowl Hh = hhHowl
  tRef <- newRef 0
  xsRef <- newRef xs
  setInterval 1000 do
    t' <- readRef tRef
    xs' <- readRef xsRef
    let cutoff = (t' + 1) * 1000
    case Lazy.span (\x -> x.time < cutoff) xs' of
      { init, rest } -> do
        writeRef tRef (t' + 1)
        writeRef xsRef rest
        for_ init \{ sample, time } ->
          setTimeout (time - t' * 1000) (play (toHowl sample))
