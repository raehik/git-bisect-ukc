{-# LANGUAGE OverloadedStrings #-}

module Main where

import Algo
import JSON
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Data.Text.IO as T

git_repo_simple_filter_test = [
    ("g", ["c2"]),          -- good
    ("b", ["c1", "g"]),     -- bad
    ("c3", ["c1"]),         -- phase 1 filtered
    ("c2", []),             -- phase 2 filtered
    ("c1", ["c2"])          -- kept
    ]

-- Initialise repo map with empty ancestors.
git_repo_list_to_map :: [(String, [String])] -> Map GitCommit ([GitCommit], Set GitCommit)
git_repo_list_to_map l = foldl (\m (c, c_parents) -> Map.insert (T.pack c) (map T.pack c_parents, Set.empty) m) Map.empty l

main = putStrLn "sup"
