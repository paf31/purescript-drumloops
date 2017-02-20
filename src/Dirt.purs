-- | The Dirt sample set

module Dirt where

import Prelude
import Audio.Howler (HOWLER, Howl, new, defaultProps)
import Control.Monad.Eff (Eff)
import Loops (Passage, beat)

dirt
  :: forall e
   . String
  -> Eff (howler :: HOWLER | e)
         { _808        :: Passage Howl
         , _808bd      :: Passage Howl
         , _808cy      :: Passage Howl
         , _808hc      :: Passage Howl
         , _808ht      :: Passage Howl
         , _808lc      :: Passage Howl
         , _808lt      :: Passage Howl
         , _808mc      :: Passage Howl
         , _808mt      :: Passage Howl
         , _808oh      :: Passage Howl
         , _808sd      :: Passage Howl
         , _909        :: Passage Howl
         , ab          :: Passage Howl
         , ade         :: Passage Howl
         , ades2       :: Passage Howl
         , ades3       :: Passage Howl
         , ades4       :: Passage Howl
         , alex        :: Passage Howl
         , alphabet    :: Passage Howl
         , amencutup   :: Passage Howl
         , armora      :: Passage Howl
         , arp         :: Passage Howl
         , arpy        :: Passage Howl
         , auto        :: Passage Howl
         , baa         :: Passage Howl
         , baa2        :: Passage Howl
         , bass        :: Passage Howl
         , bass0       :: Passage Howl
         , bass1       :: Passage Howl
         , bass2       :: Passage Howl
         , bass3       :: Passage Howl
         , bassdm      :: Passage Howl
         , bassfoo     :: Passage Howl
         , battles     :: Passage Howl
         , bd          :: Passage Howl
         , bend        :: Passage Howl
         , bev         :: Passage Howl
         , bin         :: Passage Howl
         , birds3      :: Passage Howl
         , bleep       :: Passage Howl
         , blip        :: Passage Howl
         , blue        :: Passage Howl
         , bottle      :: Passage Howl
         , breaks125   :: Passage Howl
         , breaks152   :: Passage Howl
         , breaks157   :: Passage Howl
         , breaks165   :: Passage Howl
         , breath      :: Passage Howl
         , bubble      :: Passage Howl
         , can         :: Passage Howl
         , casio       :: Passage Howl
         , cc          :: Passage Howl
         , chin        :: Passage Howl
         , chink       :: Passage Howl
         , circus      :: Passage Howl
         , clak        :: Passage Howl
         , click       :: Passage Howl
         , co          :: Passage Howl
         , cosmicg     :: Passage Howl
         , cp          :: Passage Howl
         , cr          :: Passage Howl
         , crow        :: Passage Howl
         , d           :: Passage Howl
         , db          :: Passage Howl
         , diphone     :: Passage Howl
         , diphone2    :: Passage Howl
         , dist        :: Passage Howl
         , dork2       :: Passage Howl
         , dorkbot     :: Passage Howl
         , dr          :: Passage Howl
         , dr2         :: Passage Howl
         , dr55        :: Passage Howl
         , dr_few      :: Passage Howl
         , drum        :: Passage Howl
         , drumtraks   :: Passage Howl
         , e           :: Passage Howl
         , east        :: Passage Howl
         , electro1    :: Passage Howl
         , erk         :: Passage Howl
         , f           :: Passage Howl
         , feel        :: Passage Howl
         , feelfx      :: Passage Howl
         , fest        :: Passage Howl
         , fire        :: Passage Howl
         , flick       :: Passage Howl
         , foo         :: Passage Howl
         , future      :: Passage Howl
         , gab         :: Passage Howl
         , gabba       :: Passage Howl
         , gabbaloud   :: Passage Howl
         , gabbalouder :: Passage Howl
         , glasstap    :: Passage Howl
         , glitch      :: Passage Howl
         , glitch2     :: Passage Howl
         , gretsch     :: Passage Howl
         , h           :: Passage Howl
         , hand        :: Passage Howl
         , hardcore    :: Passage Howl
         , haw         :: Passage Howl
         , hc          :: Passage Howl
         , hh          :: Passage Howl
         , hh27        :: Passage Howl
         , hit         :: Passage Howl
         , hmm         :: Passage Howl
         , ho          :: Passage Howl
         , house       :: Passage Howl
         , ht          :: Passage Howl
         , _if         :: Passage Howl
         , ifdrums     :: Passage Howl
         , incoming    :: Passage Howl
         , industrial  :: Passage Howl
         , insect      :: Passage Howl
         , invaders    :: Passage Howl
         , jazz        :: Passage Howl
         , jungbass    :: Passage Howl
         , jungle      :: Passage Howl
         , jvbass      :: Passage Howl
         , koy         :: Passage Howl
         , kurt        :: Passage Howl
         , latibro     :: Passage Howl
         , led         :: Passage Howl
         , less        :: Passage Howl
         , lighter     :: Passage Howl
         , lt          :: Passage Howl
         , made        :: Passage Howl
         , made2       :: Passage Howl
         , mash        :: Passage Howl
         , mash2       :: Passage Howl
         , metal       :: Passage Howl
         , miniyeah    :: Passage Howl
         , moan        :: Passage Howl
         , monsterb    :: Passage Howl
         , moog        :: Passage Howl
         , mouth       :: Passage Howl
         , mp3         :: Passage Howl
         , msg         :: Passage Howl
         , mt          :: Passage Howl
         , newnotes    :: Passage Howl
         , noise       :: Passage Howl
         , noise2      :: Passage Howl
         , notes       :: Passage Howl
         , numbers     :: Passage Howl
         , oc          :: Passage Howl
         , odx         :: Passage Howl
         , off         :: Passage Howl
         , pad         :: Passage Howl
         , padlong     :: Passage Howl
         , pebbles     :: Passage Howl
         , perc        :: Passage Howl
         , peri        :: Passage Howl
         , print       :: Passage Howl
         , proc        :: Passage Howl
         , procshort   :: Passage Howl
         , psr         :: Passage Howl
         , rave        :: Passage Howl
         , rave2       :: Passage Howl
         , ravemono    :: Passage Howl
         , rm          :: Passage Howl
         , sax         :: Passage Howl
         , seawolf     :: Passage Howl
         , sequential  :: Passage Howl
         , sf          :: Passage Howl
         , sheffield   :: Passage Howl
         , short       :: Passage Howl
         , sid         :: Passage Howl
         , sine        :: Passage Howl
         , sitar       :: Passage Howl
         , sn          :: Passage Howl
         , space       :: Passage Howl
         , speech      :: Passage Howl
         , speechless  :: Passage Howl
         , speedupdown :: Passage Howl
         , stab        :: Passage Howl
         , stomp       :: Passage Howl
         , subroc3d    :: Passage Howl
         , sugar       :: Passage Howl
         , sundance    :: Passage Howl
         , tabla       :: Passage Howl
         , tabla2      :: Passage Howl
         , tablex      :: Passage Howl
         , tacscan     :: Passage Howl
         , tech        :: Passage Howl
         , techno      :: Passage Howl
         , tink        :: Passage Howl
         , tok         :: Passage Howl
         , toys        :: Passage Howl
         , trump       :: Passage Howl
         , ul          :: Passage Howl
         , ulgab       :: Passage Howl
         , uxay        :: Passage Howl
         , v           :: Passage Howl
         , voodoo      :: Passage Howl
         , wind        :: Passage Howl
         , wobble      :: Passage Howl
         , world       :: Passage Howl
         , xmas        :: Passage Howl
         , yeah        :: Passage Howl
         }
dirt dir = do
  let createSample path = beat <$> new (defaultProps { urls = [dir <> path] })
  _808 <- createSample "808/CB.WAV"
  _808bd <- createSample "808bd/BD0000.WAV"
  _808cy <- createSample "808cy/CY0000.WAV"
  _808hc <- createSample "808hc/HC00.WAV"
  _808ht <- createSample "808ht/HT00.WAV"
  _808lc <- createSample "808lc/LC00.WAV"
  _808lt <- createSample "808lt/LT00.WAV"
  _808mc <- createSample "808mc/MC00.WAV"
  _808mt <- createSample "808mt/MT00.WAV"
  _808oh <- createSample "808oh/OH00.WAV"
  _808sd <- createSample "808sd/SD0000.WAV"
  _909 <- createSample "909/BT0A0A7.WAV"
  ab <- createSample "ab/000_ab2closedhh.wav"
  ade <- createSample "ade/000_011112-bassline.wav"
  ades2 <- createSample "ades2/000_01.wav"
  ades3 <- createSample "ades3/01.wav"
  ades4 <- createSample "ades4/01.wav"
  alex <- createSample "alex/000_drumx1.wav"
  alphabet <- createSample "alphabet/a.wav"
  amencutup <- createSample "amencutup/000_AMENCUT_001.wav"
  armora <- createSample "armora/000_beep.wav"
  arp <- createSample "arp/000_arp2.wav"
  arpy <- createSample "arpy/arpy01.wav"
  auto <- createSample "auto/break-kick.wav"
  baa <- createSample "baa/1.wav"
  baa2 <- createSample "baa2/1.wav"
  bass <- createSample "bass/000_bass1.wav"
  bass0 <- createSample "bass0/000_0.wav"
  bass1 <- createSample "bass1/18076__daven__01-sb-bass-hit-c.wav"
  bass2 <- createSample "bass2/69988__noizemassacre__hardcore-bass-1.wav"
  bass3 <- createSample "bass3/83245__zgump__bass-0201.wav"
  bassdm <- createSample "bassdm/000_BT0A0A7.WAV.ews"
  bassfoo <- createSample "bassfoo/000_0.wav"
  battles <- createSample "battles/000_explo1.wav"
  bd <- createSample "bd/BT0A0A7.wav"
  bend <- createSample "bend/000_2.wav"
  bev <- createSample "bev/00-mono.wav"
  bin <- createSample "bin/000_bin1.wav"
  birds3 <- createSample "birds3/000_1.wav"
  bleep <- createSample "bleep/boip.wav"
  blip <- createSample "blip/000_blipp01.wav"
  blue <- createSample "blue/aya.wav"
  bottle <- createSample "bottle/000_1.wav"
  breaks125 <- createSample "breaks125/015_sdstckbr.wav"
  breaks152 <- createSample "breaks152/000_AMEN.WAV"
  breaks157 <- createSample "breaks157/000_PLEAD.WAV"
  breaks165 <- createSample "breaks165/000_RAWCLN.WAV"
  breath <- createSample "breath/000_breath.wav"
  bubble <- createSample "bubble/000_bub0.wav"
  can <- createSample "can/000_1.wav"
  casio <- createSample "casio/high.wav"
  cc <- createSample "cc/CSHD0.wav"
  chin <- createSample "chin/000_tik1.wav"
  chink <- createSample "chink/000_chink.wav"
  circus <- createSample "circus/000_bounce.wav"
  clak <- createSample "clak/000_clak1.wav"
  click <- createSample "click/000_click0.wav"
  co <- createSample "co/CLOP1.wav"
  cosmicg <- createSample "cosmicg/000_cg_att.wav"
  cp <- createSample "cp/HANDCLP0.wav"
  cr <- createSample "cr/RIDED0.wav"
  crow <- createSample "crow/000_crow.wav"
  d <- createSample "d/000_d1.wav"
  db <- createSample "db/dbs12closedhh.wav"
  diphone <- createSample "diphone/000_kd1_002.wav"
  diphone2 <- createSample "diphone2/000_kd1_399.wav"
  dist <- createSample "dist/000_inddistb1.wav"
  dork2 <- createSample "dork2/0.wav"
  dorkbot <- createSample "dorkbot/1.wav"
  dr <- createSample "dr/000_002.WAV"
  dr2 <- createSample "dr2/000_DR110CHT.WAV"
  dr55 <- createSample "dr55/000_DR55 hi hat.wav"
  dr_few <- createSample "dr_few/000_001.WAV"
  drum <- createSample "drum/000_drum1.wav"
  drumtraks <- createSample "drumtraks/000_DT Cabasa.wav"
  e <- createSample "e/000_e1.wav"
  east <- createSample "east/nipon_wood_block.wav"
  electro1 <- createSample "electro1/000_et1closedhh.wav"
  erk <- createSample "erk/000_123.wav"
  f <- createSample "f/000_f.wav"
  feel <- createSample "feel/BD 04 d.wav"
  feelfx <- createSample "feelfx/blnk.wav"
  fest <- createSample "fest/000_foo.wav"
  fire <- createSample "fire/fire.wav"
  flick <- createSample "flick/000_square-p.wav"
  foo <- createSample "foo/000_samthfdbrk.wav"
  future <- createSample "future/000_808KICK4.wav"
  gab <- createSample "gab/gab01.wav"
  gabba <- createSample "gabba/000_0.wav"
  gabbaloud <- createSample "gabbaloud/000_0.wav"
  gabbalouder <- createSample "gabbalouder/000_0.wav"
  glasstap <- createSample "glasstap/000_0.wav"
  glitch <- createSample "glitch/000_BD.wav"
  glitch2 <- createSample "glitch2/000_BD.wav"
  gretsch <- createSample "gretsch/brushhitom.wav"
  h <- createSample "h/0_da0-200%_1000_0_R.wav"
  hand <- createSample "hand/hand1-mono.wav"
  hardcore <- createSample "hardcore/000_hcclosedhh.wav"
  haw <- createSample "haw/hawaiian-hh.wav"
  hc <- createSample "hc/HHCD0.wav"
  hh <- createSample "hh/000_hh3closedhh.wav"
  hh27 <- createSample "hh27/000_hh27closedhh.wav"
  hit <- createSample "hit/bandpass-blart.wav"
  hmm <- createSample "hmm/hmm.wav"
  ho <- createSample "ho/HHOD0.wav"
  house <- createSample "house/000_BD.wav"
  ht <- createSample "ht/HT0D0.wav"
  _if <- createSample "if/gab.wav"
  ifdrums <- createSample "ifdrums/ignorebd.wav"
  incoming <- createSample "incoming/000_Mattel  Snare.wav"
  industrial <- createSample "industrial/000_01.wav"
  insect <- createSample "insect/000_everglades_conehead.wav"
  invaders <- createSample "invaders/000_0.wav"
  jazz <- createSample "jazz/000_BD.wav"
  jungbass <- createSample "jungbass/deeep_n_low.wav"
  jungle <- createSample "jungle/jungle4closedhh.wav"
  jvbass <- createSample "jvbass/000_01.wav"
  koy <- createSample "koy/01_left.wav"
  kurt <- createSample "kurt/000_kurt01.wav"
  latibro <- createSample "latibro/000_Sound2.wav"
  led <- createSample "led/000_foo.wav"
  less <- createSample "less/bass2.wav"
  lighter <- createSample "lighter/000_0.wav"
  lt <- createSample "lt/LT0D0.wav"
  made <- createSample "made/0.wav"
  made2 <- createSample "made2/output.wav"
  mash <- createSample "mash/0.wav"
  mash2 <- createSample "mash2/output.wav"
  metal <- createSample "metal/000_0.wav"
  miniyeah <- createSample "miniyeah/000_Sound0.wav"
  moan <- createSample "moan/000_0_moan2.wav"
  monsterb <- createSample "monsterb/000_jumpdown.wav"
  moog <- createSample "moog/000_Mighty Moog C2.wav"
  mouth <- createSample "mouth/000_1.wav"
  mp3 <- createSample "mp3/000_mp30.wav"
  msg <- createSample "msg/000_msg0.wav"
  mt <- createSample "mt/MT0D0.wav"
  newnotes <- createSample "newnotes/000_0.wav"
  noise <- createSample "noise/000_noise.wav"
  noise2 <- createSample "noise2/000_0.wav"
  notes <- createSample "notes/000_0.wav"
  numbers <- createSample "numbers/0.wav"
  oc <- createSample "oc/OPCL1.wav"
  odx <- createSample "odx/000_Kick_1.wav"
  off <- createSample "off/000_01.wav"
  pad <- createSample "pad/alien-monolith-pad.wav"
  padlong <- createSample "padlong/atmospheric-abduction.wav"
  pebbles <- createSample "pebbles/90788__kmoon__pebbles-scrape-drag-foot.wav"
  perc <- createSample "perc/000_perc0.wav"
  peri <- createSample "peri/bd-rev.wav"
  print <- createSample "print/0.wav"
  proc <- createSample "proc/000_2.wav"
  procshort <- createSample "procshort/000_1.wav"
  psr <- createSample "psr/000_01.wav"
  rave <- createSample "rave/AREUREADY.wav"
  rave2 <- createSample "rave2/electric_ping01.ogg"
  ravemono <- createSample "ravemono/Babylon.wav"
  rm <- createSample "rm/RIM0.wav"
  sax <- createSample "sax/000_notes121a.wav"
  seawolf <- createSample "seawolf/000_minehit.wav"
  sequential <- createSample "sequential/000_Tom Clap.wav"
  sf <- createSample "sf/000_bass.wav"
  sheffield <- createSample "sheffield/jakeinsects.wav"
  short <- createSample "short/sampleoftheday-gtt-fx-synth-009.wav"
  sid <- createSample "sid/000_bas2.wav"
  sine <- createSample "sine/000_sine.wav"
  sitar <- createSample "sitar/000_d_maj_sitar_chorda.wav"
  sn <- createSample "sn/ST0T0S0.wav"
  space <- createSample "space/000_0.wav"
  speech <- createSample "speech/000_Sound10.wav"
  speechless <- createSample "speechless/......wav"
  speedupdown <- createSample "speedupdown/000_Sound20.wav"
  stab <- createSample "stab/000_stab1.wav"
  stomp <- createSample "stomp/000_0.wav"
  subroc3d <- createSample "subroc3d/000_01.wav"
  sugar <- createSample "sugar/000_bark.wav"
  sundance <- createSample "sundance/000_bong.wav"
  tabla <- createSample "tabla/000_bass_flick1.wav"
  tabla2 <- createSample "tabla2/23645_loofa_A_001.wav"
  tablex <- createSample "tablex/0.wav"
  tacscan <- createSample "tacscan/000_01.wav"
  tech <- createSample "tech/tn1closedhh.wav"
  techno <- createSample "techno/000_0.wav"
  tink <- createSample "tink/000_tink1.wav"
  tok <- createSample "tok/000_0.wav"
  toys <- createSample "toys/ClassicalMusic-Notes.wav"
  trump <- createSample "trump/tightstabb.wav"
  ul <- createSample "ul/beep.wav"
  ulgab <- createSample "ulgab/gab1.wav"
  uxay <- createSample "uxay/000_bar.wav"
  v <- createSample "v/000_b_blipp01.wav"
  voodoo <- createSample "voodoo/000_VoodooBass.wav"
  wind <- createSample "wind/000_wind1.wav"
  wobble <- createSample "wobble/000_0.wav"
  world <- createSample "world/bd.wav"
  xmas <- createSample "xmas/170535__cognito-perceptu__merry-christmas.wav"
  yeah <- createSample "yeah/000_Sound0.wav"
  pure { _808
       , _808bd
       , _808cy
       , _808hc
       , _808ht
       , _808lc
       , _808lt
       , _808mc
       , _808mt
       , _808oh
       , _808sd
       , _909
       , ab
       , ade
       , ades2
       , ades3
       , ades4
       , alex
       , alphabet
       , amencutup
       , armora
       , arp
       , arpy
       , auto
       , baa
       , baa2
       , bass
       , bass0
       , bass1
       , bass2
       , bass3
       , bassdm
       , bassfoo
       , battles
       , bd
       , bend
       , bev
       , bin
       , birds3
       , bleep
       , blip
       , blue
       , bottle
       , breaks125
       , breaks152
       , breaks157
       , breaks165
       , breath
       , bubble
       , can
       , casio
       , cc
       , chin
       , chink
       , circus
       , clak
       , click
       , co
       , cosmicg
       , cp
       , cr
       , crow
       , d
       , db
       , diphone
       , diphone2
       , dist
       , dork2
       , dorkbot
       , dr
       , dr2
       , dr55
       , dr_few
       , drum
       , drumtraks
       , e
       , east
       , electro1
       , erk
       , f
       , feel
       , feelfx
       , fest
       , fire
       , flick
       , foo
       , future
       , gab
       , gabba
       , gabbaloud
       , gabbalouder
       , glasstap
       , glitch
       , glitch2
       , gretsch
       , h
       , hand
       , hardcore
       , haw
       , hc
       , hh
       , hh27
       , hit
       , hmm
       , ho
       , house
       , ht
       , _if
       , ifdrums
       , incoming
       , industrial
       , insect
       , invaders
       , jazz
       , jungbass
       , jungle
       , jvbass
       , koy
       , kurt
       , latibro
       , led
       , less
       , lighter
       , lt
       , made
       , made2
       , mash
       , mash2
       , metal
       , miniyeah
       , moan
       , monsterb
       , moog
       , mouth
       , mp3
       , msg
       , mt
       , newnotes
       , noise
       , noise2
       , notes
       , numbers
       , oc
       , odx
       , off
       , pad
       , padlong
       , pebbles
       , perc
       , peri
       , print
       , proc
       , procshort
       , psr
       , rave
       , rave2
       , ravemono
       , rm
       , sax
       , seawolf
       , sequential
       , sf
       , sheffield
       , short
       , sid
       , sine
       , sitar
       , sn
       , space
       , speech
       , speechless
       , speedupdown
       , stab
       , stomp
       , subroc3d
       , sugar
       , sundance
       , tabla
       , tabla2
       , tablex
       , tacscan
       , tech
       , techno
       , tink
       , tok
       , toys
       , trump
       , ul
       , ulgab
       , uxay
       , v
       , voodoo
       , wind
       , wobble
       , world
       , xmas
       , yeah
       }
