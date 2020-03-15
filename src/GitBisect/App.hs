module GitBisect.App where

import GitBisect.Types
import qualified GitBisect.Algo as Algo
import qualified GitBisect.Parser as P
import Data.Text (Text)
import Control.Monad.Trans.Except
import qualified Data.Map as Map
import Data.Map (Map)
import qualified System.Random as Random
import Control.Monad.State.Lazy

data Error
    = ErrorUnspecified
    deriving (Show)

data ProviderError
    = ProviderErrorQueryCommitIDNotPresent CommitID Text Text
    deriving (Show)

data SelectorError
    = SelectorErrorUnspecified
    deriving (Show)

type RepoSolution = (Text, [InstanceSolution])
type InstanceSolution = (Text, Either Error Solution)
type Solution = (CommitID, Int)

-- | The application's environment.
data Env = Env {
    envProvider :: Provider,
    envSolver :: Solver
}

--class Provider where
--    query :: (Provider p) => p -> CommitID -> Either Error CommitStatus

-- | Either a commit to query, or a commit to submit as a solution.
data CommitSelection
    = CommitQuestion CommitID
    | CommitSolution CommitID
    deriving (Show)

-- | The problem provider.
data Provider = Provider {
    providerInit :: (),
    providerQueryCommit :: CommitID -> CommitGraph -> ExceptT ProviderError IO CommitStatus
}

-- | The problem solver.
data Solver = Solver {
    solverSelectCommit :: CommitID -> CommitGraph -> ExceptT SelectorError IO CommitSelection
}
-}

--------------------------------------------------------------------------------

selectMockImmediateSolveRandom :: CommitID -> CommitGraph -> Either SelectorError CommitSelection
selectMockImmediateSolveHead head graph = Right $ CommitSolution head

selectMockImmediateSolveRandom :: CommitID -> CommitGraph -> IO (Either SelectorError CommitSelection)
selectMockImmediateSolveRandom head graph = do
    let commits = Map.size graph
    rnd <- Random.randomRIO (0, commits-1)
    let cBisect = Map.keys graph !! rnd
    return $ Right $ CommitSolution cBisect

data ProviderEnv = ProviderEnv {
    providerEnvRepos :: [P.GitRepo]
}
