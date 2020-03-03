module GitBisect.Types where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)

type GitCommit = Text
type GitGraph = Map GitCommit GitGraphEntry
data GitGraphEntry = GitGraphEntry {
    git_graph_entry_parents :: [GitCommit],
    git_graph_entry_ancestors :: Maybe (Set GitCommit)
} deriving (Show)
