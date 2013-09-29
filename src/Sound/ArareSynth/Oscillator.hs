{-# LANGUAGE RankNTypes, ImpredicativeTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Sound.ArareSynth.Oscillator where
import Data.Vector ((!), Vector(..), fromList)
import qualified Data.Vector as V
import Data.Fixed (mod')
import Data.Maybe (fromMaybe, listToMaybe)
import Sound.ArareSynth.Types
import Sound.Sarasvati

-------------------
-- type definition

class Oscillator o where
  oscillateF :: Channel c h => o c -> Frequency -> (o c, c)

oscillate :: (Oscillator o, Channel c h) => o c -> [Frequency] -> [c]
oscillate _ [] = []
oscillate osci (x:xs) = let
  (nosci, v) = oscillateF osci x
  in v : oscillate nosci xs

-----

data WaveTable c = WaveTable
  { wtERange :: Float
  , wtStep :: Int
  , wtLength :: Int
  , wtTable :: Vector c
  } deriving (Show, Eq)
instance Oscillator WaveTable where
  oscillateF osci freq = let
    nosci = osci 
      { wtERange = (wtERange osci + freq `mod'` 1) `mod'` 1
      , wtStep = (wtStep osci + floor freq) `mod` wtLength osci
      }
    in (nosci, wtTable osci ! wtStep osci)

-------------------
-- sin

sinList :: SampleRate -> [Float]
sinList s = take (floor s) $ map sin [0, step s..]
  where 
    step :: Double -> Float
    step d = pi / (realToFrac d / 2)

sinOscillator :: Channel c h => SampleRate -> WaveTable c
sinOscillator s = let
  len = floor s
  in WaveTable 
    { wtERange = 0
    , wtStep = 0
    , wtLength = len
    , wtTable = fromList $ (makeWave $ sinList s)
    }

-------------------
-- test program (must delete)

osci = sinOscillator 44100 :: WaveTable Stereo
wave f = oscillate osci $ replicate 100000 f
test1 = sarasvatiOutput defaultConfig (wave $! 440)
test2 = sarasvatiOutput defaultConfig (wave 440 /+/ wave 880)
