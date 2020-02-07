-- TODO:
--   * Map vs. HashMap (unclear) https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure
--

import Data.Map (Map)
import qualified Data.Map as Map

git_repo_01 = [
    ("a", []),
    ("b", ["a"]),
    ("c", ["b"]),
    ("d", ["c"]),
    ("e", ["c", "d"]),
    ("f", ["b", "d", "e"]),
    ("g", ["f"])
    ]

--git_repo_to_internal :: [(String, [String])] -> Map String ([String], [String])
git_repo_to_internal c_bad l = git_nodelist_to_internal' (git_repo_subgraph c_bad (Map.fromList l)) Map.empty
git_repo_to_internal' [] m = m
git_repo_to_internal' ((c, cps):cs) m =
    -- for cp in cps, update cp in m
    -- then update c in m
    let    map_updated_children = foldl (map_list_merge c) m cps
    in let map_next = Map.insertWith map_value_concat c (cps, []) map_updated_children
    in     git_repo_to_internal' cs map_next

map_list_merge c m cp = Map.insertWith map_value_concat cp ([], [c]) m
map_value_concat (p1, c1) (p2, c2) = (foldr (:) p1 p2, foldr (:) c1 c2)

git_repo_subgraph c g = git_repo_subgraph' g Map.empty [c]
git_repo_subgraph' g sg [] = sg
git_repo_subgraph' g sg (c:cs) =
    git_repo_subgraph g (cs

