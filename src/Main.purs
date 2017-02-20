module Main where

import Prelude
import Audio.Howler (new, defaultProps)
import Data.Foldable (fold)
import Data.Rational (fromInt)
import Loops (Audio, Dur(..), Passage, beat, loop, merge, playWith, silence, swing, track, warp)

data Sample = Bd | Sn | Hh

bd :: Passage Sample
bd = beat Bd

sn :: Passage Sample
sn = beat Sn

hh :: Passage Sample
hh = beat Hh

si :: Passage Sample
si = silence

main :: Audio () Unit
main = void do
  bdHowl <- new (defaultProps { urls = ["/wav/bd.wav"], volume = 1.0 })
  snHowl <- new (defaultProps { urls = ["/wav/sn.wav"], volume = 0.75 })
  hhHowl <- new (defaultProps { urls = ["/wav/hh.wav"], volume = 1.0 })
  let toHowl Bd = bdHowl
      toHowl Sn = snHowl
      toHowl Hh = hhHowl
      p1 = fold [bd, bd, si, bd, sn, bd, si, bd, bd, bd, si, bd, bd, bd, bd, bd]
      p2 = fold [hh, hh, hh, hh, hh, hh, hh, hh, hh, hh, hh, hh, si, hh, si, hh]
      together = merge p1 p2
      looped = loop (BPM $ fromInt 240) together
      warped = warp (swing 20.0) looped
  playWith toHowl (track warped)
