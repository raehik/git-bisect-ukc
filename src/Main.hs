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

git_repo_01 = [
    ("a", []), -- good
    ("b", ["a"]),
    ("c", ["b"]),
    ("d", ["c"]),
    ("e", ["c", "d"]),
    ("f", ["b", "d", "e"]),
    ("g", ["f"]), -- bad
    ("x", ["g"]) -- this should be filtered
    ]

-- Initialise repo map with empty ancestors.
git_repo_list_to_map :: [([Char], [[Char]])] -> Map GitCommit ([GitCommit], Set GitCommit)
git_repo_list_to_map l = foldl (\m (c, c_parents) -> Map.insert (T.pack c) (map T.pack c_parents, Set.empty) m) Map.empty l

main = putStrLn "sup"
