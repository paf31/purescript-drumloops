module Main where

import Prelude
import Data.Newtype (wrap)
import Loops (Audio, bd, sn, hh, silence, loop, track)

main :: Audio () Unit
main = void do
  track $ loop (wrap 120) $ bd <> sn <> sn <> sn
  track $ loop (wrap 120) $ hh <> hh <> silence
