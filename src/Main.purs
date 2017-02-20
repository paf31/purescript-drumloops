module Main where

import Prelude
import Dirt (dirt)
import Data.Foldable (fold)
import Data.Rational (fromInt)
import Loops (Audio, Dur(BPM), loop, merge, play, silence, swing, track, warp)

main :: Audio () Unit
main = void do
  { _808bd: bd, _808sd: sn, _808hc: hh } <- dirt "../Dirt-Samples/"
  let si = silence
      p1 = fold [bd, bd, si, bd, sn, bd, si, bd]
      p2 = fold [hh, hh, hh, hh, hh, hh, hh, hh]
      together = merge p1 p2
      looped = loop (BPM $ fromInt 240) together
      warped = warp (swing 20.0) looped
  play (track warped)
