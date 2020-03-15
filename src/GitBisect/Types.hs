module GitBisect.Types where

import Data.Set (Set)
import Data.Map (Map)
import Data.Text (Text)

type CommitID = Text
type CommitGraph = Map CommitID CommitGraphEntry
data CommitGraphEntry = CommitGraphEntry {
    commitGraphEntryParents :: [CommitID],
    commitGraphEntryAncestors :: Maybe (Set CommitID)
} deriving (Show)
data CommitStatus
    = CommitGood
    | CommitBad
    deriving (Show)
