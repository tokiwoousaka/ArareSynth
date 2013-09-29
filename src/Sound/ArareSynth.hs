module Sound.ArareSynth
  ( module Sound.ArareSynth
  , module Sound.ArareSynth.Oscillator
  , module Sound.ArareSynth.Types
  )  where
import Control.DeepSeq
import Sound.ArareSynth.Oscillator
import Sound.ArareSynth.Types
import Sound.Sarasvati

data ArareConfig = ArareConfig

arareDefault :: ArareConfig
arareDefault = ArareConfig

ararePlay :: Channel c h => ArareConfig -> [c] -> IO (Either Error ())
ararePlay _ w = sarasvatiOutput defaultConfig $!! w
