-- TODO:
--   * Map vs. HashMap (unclear) https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure
--

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (mapM_)

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

--git_repo_to_internal :: [(String, [String])] -> Map String ([String], [String])
--git_repo_to_internal c_bad l = git_repo_to_internal' (git_repo_subgraph c_bad (Map.fromList l)) Map.empty
--git_repo_to_internal' [] m = m
--git_repo_to_internal' ((c, cps):cs) m =
    -- for cp in cps, update cp in m
    -- then update c in m
--    let    map_updated_children = foldl (map_list_merge c) m cps
--    in let map_next = Map.insertWith map_value_concat c (cps, []) map_updated_children
--    in     git_repo_to_internal' cs map_next

git_repo_calculate_children g = Map.foldrWithKey git_repo_map_fold_children g g

--git_repo_map_fold_children c (cps, ccs) m = Map.insertWith map_value_concat c (cps, []) (foldl (map_list_merge c) m cps)
git_repo_map_fold_children c (cps, ccs) m = foldl (map_list_merge c) m cps

map_list_merge c m cp = Map.insertWith map_value_concat cp ([], [c]) m
map_value_concat (p1, c1) (p2, c2) = (foldr (:) p1 p2, foldr (:) c1 c2)

--git_repo_subgraph :: [String] -> String -> Map a ([a], [a]) -> Map a ([a], [a])
git_repo_subgraph c_good c_bad g = git_repo_subgraph' c_good g Set.empty [c_bad]
git_repo_subgraph' _ g gc [] = foldl (map_copy_from_map g) Map.empty gc
git_repo_subgraph' c_good g gc (c:cs) =
    case Map.lookup c g of
        Just (cps, ccs) -> git_repo_subgraph' c_good g (Set.insert c gc) (revprepend_list cps cs)
        Nothing -> git_repo_subgraph' c_good g gc cs

-- Copy entry k from map m1 into map m2 if it exists.
-- Intended for use with foldl.
map_copy_from_map m1 m2 k =
    case Map.lookup k m1 of
        Just v -> Map.insert k v m2
        Nothing -> m2

-- Initialise repo map (without calculating children yet)
git_repo_list_to_map l = git_repo_list_to_map' Map.empty l
git_repo_list_to_map' g [] = g
git_repo_list_to_map' g ((c, cps):cs) = git_repo_list_to_map' (Map.insert c (cps, []) g) cs

revprepend_list [] y = y
revprepend_list (x:xs) y = revprepend_list xs (x:y)

--git_repo_to_string g = map (++ "\n" . git_commit_to_string) (Map.assocs g)
git_repo_to_string g = Map.foldrWithKey (\k x ks -> ks ++ git_commit_to_string k x ++ "\n") "" g
--git_repo_print g = mapM_ git_commit_print (Map.assocs g)

git_commit_to_string c (cps, ccs) =
    c
    ++ "\n  P: " ++ show cps
    ++ "\n  C: " ++ show ccs
--git_commit_print (c, (cps, ccs)) = print c
