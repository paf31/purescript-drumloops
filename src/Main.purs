-- localhost:8000/html/
module Main where

import Prelude
import Data.Newtype (wrap)
import Loops (Audio, bd, sn, hh, silence, loop, track, play, merge)
import Data.Foldable (fold)
import Data.Rational (fromInt)

si = silence

main :: Audio (_) Unit
main = void do
  let p1 = fold [bd,si,si,bd]
      p2 = fold [si,hh,sn,si,hh,hh]
  play $ track $ loop (wrap $ fromInt 180) $ merge p1 p2
    -- Data.Foldable.fold :: forall f m. (Foldable f, Monoid m) => f m -> m
      -- ? In this case, f=Array, but what is m?
