-- localhost:8000/html/
module Main where

import Prelude
import Data.Newtype (wrap)
import Loops (Audio, unsafeWarp, silence, bd, sn, hh, Dur(..), loop, track, play, merge)
import Data.Foldable (fold)
import Data.Rational (fromInt)
import Math (sin,pi)

si = silence

main :: Audio () Unit
main = void do
  let p1 = fold [bd,bd,sn,si]
      p2 = fold [si,hh,sn,si,hh,hh]
    -- Data.Foldable.fold :: forall f m. (Foldable f, Monoid m) => f m -> m
      -- ? In this case, f=Array, but what is m?
      -- m = Passage
  play $ track $ unsafeWarp (\x -> x + sin (2.0*pi*x) / 10.0) $ loop (BPM $ fromInt 240) $ merge (fold [hh,hh,hh,hh]) p1
  -- play $ track $ unsafeWarp (\x -> x + sin (2.0*pi*x) / 10.0) $ loop (BPM $ fromInt 240) $ merge p2 p1 --