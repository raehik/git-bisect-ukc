module GitBisect.Types where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)

type GitCommit = Text
type GitGraph = Map GitCommit GitGraphEntry
data GitGraphEntry = GitGraphEntry {
    gitGraphEntryParents :: [GitCommit],
    gitGraphEntryAncestors :: Maybe (Set GitCommit)
} deriving (Show)
