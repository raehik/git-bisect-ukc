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

-- Initialise repo map with empty ancestors.
git_repo_list_to_map :: [(String, [String])] -> GitGraph
git_repo_list_to_map l = foldl (\m (c, c_parents) -> Map.insert (T.pack c) (GitGraphEntry (map T.pack c_parents) Nothing) m) Map.empty l

main = putStrLn "sup"
