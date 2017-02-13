module Loops
  ( Sample (..)
  , Passage
  , merge
  , silence
  , bd
  , sn
  , hh
  , Millis
  , Dur (..)
  , Event
  , Track
  , unsafeWarp
  , loop
  , Audio
  , Playable
  , track
  , play
  , degrade
  ) where

import Prelude
import Data.List.Lazy as Lazy
import Audio.Howler (HOWLER, defaultProps, new, play) as H
import Data.Rational (Rational, (%), toNumber, fromInt)
import Data.Int (round)
import Data.Int (toNumber) as Int
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF, newRef, readRef, writeRef)
import Control.Monad.Eff.Timer (TIMER, setInterval, setTimeout)
import Data.Newtype (wrap, class Newtype, over, over2)
import Data.Foldable (for_)
import Data.List (List(..), (:), singleton)
import Data.Monoid (class Monoid, mempty)

import Control.Monad.Eff.Random (RANDOM, random)

-- | Requires a monotonic increasing function of numbers
unsafeWarp :: (Number -> Number) -> Track -> Track
unsafeWarp f = over Track $ map \{sample,time} -> {sample, time: f' time}
  where f' t = round $ x * 1000.0
          where x = f $ Int.toNumber t / 1000.0

data Sample = Bd | Sn | Hh

newtype Passage = Passage
  { length :: Rational
  , values :: List { sample :: Sample, offset :: Rational }
  }

type PassageUnsized = List { sample :: Sample, offset :: Rational }

shuffleKeepingSort :: PassageUnsized -> PassageUnsized -> PassageUnsized
shuffleKeepingSort (Nil) x = x
shuffleKeepingSort x (Nil) = x
shuffleKeepingSort a@({sample: sa, offset: oa} : moreAs)
                   b@({sample: sb, offset: ob} : moreBs)
  | oa < ob = {sample: sa, offset: oa} : shuffleKeepingSort moreAs b
  | otherwise = {sample: sb, offset: ob} : shuffleKeepingSort a moreBs

merge :: Passage -> Passage -> Passage
merge (Passage a) (Passage b) = Passage
  { length: max a.length b.length
  , values: shuffleKeepingSort a.values b.values
  }

newtype Merge = Merge Passage

derive instance newtypeMerge :: Newtype Merge _
instance semigroupMerge :: Semigroup Merge where
  append = over2 Merge merge

instance monoidMerge :: Monoid Merge where
  mempty = wrap mempty

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

silence :: Passage
silence =
  Passage { length: 1%1
       , values: mempty
       }

beat :: Sample -> Passage
beat s =
  Passage { length: 1%1
       , values: singleton
           { offset: 0%1
           , sample: s
           }
       }

bd :: Passage
bd = beat Bd

sn :: Passage
sn = beat Sn

hh :: Passage
hh = beat Hh

type Millis = Int
type Event = { sample :: Sample, time :: Millis }
newtype Track = Track (Lazy.List Event)
derive instance newtypeTrack :: Newtype Track _
data Dur = Dur Rational | BPM Rational

durMs :: Dur -> Rational
durMs (Dur x) = x * fromInt 1000
durMs (BPM x) = fromInt 60000 / x

loop :: Dur -> Passage -> Track  -- ! assumes samples are ordered
loop dur (Passage { length, values } ) = Track do
  n <- Lazy.iterate (_ + 1) 0
  Lazy.fromFoldable $ map (\ { sample, offset } ->
      { sample
      , time: round $ toNumber $ durMs dur
          * (fromInt n * length + offset) -- ! max: 596 hr
      })
    values

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
