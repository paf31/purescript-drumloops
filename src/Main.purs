module Main where

import Prelude
import Data.Foldable (fold)
import Data.Newtype (wrap)
import Data.Rational (fromInt)
import Loops (Audio, unsafeWarp, silence, bd, sn, hh, Dur(..), loop, track, play, merge)
import Math (sin,pi)

si = silence

main :: Audio () Unit
main = void do
  let p1 = fold [bd,bd,sn,si]
      p2 = fold [si,hh,sn,si,hh,hh]
  play $ track $ unsafeWarp (\x -> x + sin (2.0*pi*x) / 10.0) $ loop (BPM $ fromInt 240) $ merge (fold [hh,hh,hh,hh]) p1
