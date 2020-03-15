module GitBisect.Examples where

import GitBisect.Types
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as T

--exampleToMap l =
--    foldl (\m (c, cps) -> Map.insert (T.pack c) (CommitGraphEntry (map T.pack cps) Nothing) m) Map.empty l

meh = [
    ("a", []), -- good
    ("b", ["a"]),
    ("c", ["b"]),
    ("d", ["c"]),
    ("e", ["c", "d"]),
    ("f", ["b", "d", "e"]),
    ("g", ["f"]), -- bad
    ("x", ["g"]) -- this should be filtered
    ]

simpleFilterTest = [
    ("g", ["c2"]),          -- good
    ("b", ["c1", "g"]),     -- bad
    ("c2", []),             -- filtered during deletion
    ("c3", ["c1"]),         -- filtered during subgraphing
    ("c1", ["c2"])          -- kept
    ]
