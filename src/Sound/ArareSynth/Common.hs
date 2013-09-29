module Sound.ArareSynth.Common where
import Control.Monad.Free

-------------------
-- F to list

listFromF :: Free ((,) a) b -> ([a], b)
listFromF (Pure x) = ([], x)
listFromF _ = undefined
