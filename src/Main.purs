module Main where

import Prelude
import Data.Foldable (fold)
import Data.Rational (fromInt)
import Loops (Audio, Dur(..), Passage, bd, hh, loop, merge, play, silence, sn, swing, track, warp)

si :: Passage
si = silence

main :: Audio () Unit
main = void do
  let p1 = fold [bd, bd, sn, si]
      p2 = fold [hh, hh, hh, hh]
      together = merge p1 p2
      looped = loop (BPM $ fromInt 240) together
      warped = warp (swing 8.0) looped
  play $ track warped
