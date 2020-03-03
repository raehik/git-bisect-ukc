{-# LANGUAGE OverloadedStrings #-}

module GitBisect.Tests where

import Data
import Algo
import Main
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

git_repo_ex_meh = [
    ("a", []), -- good
    ("b", ["a"]),
    ("c", ["b"]),
    ("d", ["c"]),
    ("e", ["c", "d"]),
    ("f", ["b", "d", "e"]),
    ("g", ["f"]), -- bad
    ("x", ["g"]) -- this should be filtered
    ]

git_repo_ex_simple_filter_test = [
    ("g", ["c2"]),          -- good
    ("b", ["c1", "g"]),     -- bad
    ("c2", []),             -- phase 1 filtered
    ("c3", ["c1"]),         -- phase 2 filtered
    ("c1", ["c2"])          -- kept
    ]
