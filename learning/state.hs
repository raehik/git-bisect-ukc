import Control.Monad.Trans.State
import System.Random

type App = ReaderT Env IO

data Env = Env {
    cStateRng :: StdGen,
    cStateCounter :: Int
} deriving (Show)

--rngFromZeroTo upper = state $ randomR (0, upper)

--initCState = state $ CState { cStateRng = mkStdGen 0, cStateCounter = 0 }
initCState = CState { cStateRng = mkStdGen 0, cStateCounter = 0 }

--inc s = cStateCounter
