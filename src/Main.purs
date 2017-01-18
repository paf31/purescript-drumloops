module Main where

import Prelude
import Audio.Howler (HOWLER, defaultProps, new, play)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer (IntervalId, TIMER, setInterval, setTimeout)
import Data.Foldable (for_)

data Sample = Bd | Sn | Hh

type Millis = Int

type Phrase = Array { sample :: Sample, time :: Millis }

phrase :: Phrase
phrase =
  [ { sample: Bd
    , time: 0
    }
  , { sample: Hh
    , time: 125
    }
  , { sample: Sn
    , time: 250
    }
  , { sample: Hh
    , time: 375
    }
  , { sample: Hh
    , time: 625
    }
  , { sample: Sn
    , time: 750
    }
  , { sample: Hh
    , time: 875
    }
  ]

loop :: forall e. Millis -> Phrase -> Eff (howler :: HOWLER, timer :: TIMER | e) IntervalId
loop duration p = do
  bd <- new (defaultProps { urls = ["/wav/bd.wav"], volume = 1.0 })
  sn <- new (defaultProps { urls = ["/wav/sn.wav"], volume = 0.75 })
  hh <- new (defaultProps { urls = ["/wav/hh.wav"], volume = 0.25 })
  let toHowl Bd = bd
      toHowl Sn = sn
      toHowl Hh = hh
  setInterval duration do
    for_ p \{ sample, time } ->
      setTimeout time (play (toHowl sample))

main :: forall e. Eff (howler :: HOWLER, timer :: TIMER | e) Unit
main = void do
  loop 1000 phrase
