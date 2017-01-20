module Main where

import Prelude
import Audio.Howler (HOWLER)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Timer (TIMER)
import Data.Newtype (wrap)
import Loops (bd, sn, hh, silence, loop, track)

main :: Eff ( howler :: HOWLER
            , ref    :: REF
            , timer  :: TIMER
            ) Unit
main = void do
  track $ loop (wrap 120) $ bd <> sn <> sn <> sn
  track $ loop (wrap 120) $ hh <> hh <> silence
