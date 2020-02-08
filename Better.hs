-- TODO:
--   * Map vs. HashMap (unclear) https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure
--

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (mapM_)
import qualified Data.Dequeue as Dequeue

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

git_repo_calculate_children g = Map.foldrWithKey git_repo_map_fold_children g g
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

git_repo_to_string g = Map.foldrWithKey (\k x ks -> ks ++ git_commit_to_string k x ++ "\n") "" g

git_commit_to_string c (cps, ccs) =
    c
    ++ "\n  P: " ++ show cps
    ++ "\n  C: " ++ show ccs

--git_repo_calculate_descendants_and_get_heads :: a -> Map a ([a], [a]) -> (Map a Int, [a])
git_repo_calculate_descendants_and_get_heads c_bad g =
    git_repo_calculate_descendants_and_get_heads' g (Map.singleton c_bad 1, []) (Dequeue.pushBack Dequeue.empty c_bad)
git_repo_calculate_descendants_and_get_heads' :: Ord a => Map a ([a], [a]) -> (Map a Int, [a]) -> Dequeue.BankersDequeue a -> (Map a Int, [a])
git_repo_calculate_descendants_and_get_heads' g (ranks, heads) queue =
    case Dequeue.popFront queue of
        Nothing -> (ranks, heads)
        Just (c, queue') ->
            case Map.lookup c g of
                -- TODO: this Nothing is an error, should be unreachable
                Nothing -> git_repo_calculate_descendants_and_get_heads' g (ranks, heads) queue'
                Just ([], _) ->
                    git_repo_calculate_descendants_and_get_heads' g (ranks, (c:heads)) queue'
                Just (cps, _) ->
                    case Map.lookup c ranks of
                        -- TODO: again, weird error case
                        Nothing -> git_repo_calculate_descendants_and_get_heads' g (ranks, heads) queue'
                        Just rank ->
                            let (ranks', queue'') = update_ranks_and_queue cps (rank+1) ranks queue'
                            in  git_repo_calculate_descendants_and_get_heads' g (ranks', heads) queue''

--update_ranks_and_queue :: Ord a => [a] -> Int -> Map a Int -> Dequeue.BankersDequeue a -> (Map a Int, Dequeue.BankersDequeue a)
update_ranks_and_queue [] rank ranks q = (ranks, q)
update_ranks_and_queue (cp:cps) rank ranks q =
    case Map.lookup cp ranks of
        Nothing -> update_ranks_and_queue cps rank (Map.insert cp rank ranks) (Dequeue.pushBack q cp)
        Just _ -> update_ranks_and_queue cps rank ranks q

-- TODO: possible speedup here, to end even earlier when calculating child ranks
-- (instead of when we actually get to that child)
git_repo_get_best_bisect_commit g ranks (h:hs) =
    git_repo_get_best_bisect_commit' g ranks (h, 1) (push_back_list_to_queue (h:hs) Dequeue.empty :: Dequeue.BankersDequeue String)
git_repo_get_best_bisect_commit' g ranks (c_best, c_best_rank) queue =
    case Dequeue.popFront queue of
        Nothing -> c_best
        Just (c, queue') ->
            case Map.lookup c ranks of
                -- TODO: this Nothing is an error, should be unreachable
                Nothing -> git_repo_get_best_bisect_commit' g ranks (c_best, c_best_rank) queue'
                Just rank ->
                    if rank <= floor ((fromIntegral (Map.size g))/2)
                    then c
                    else
                        let (c_best', c_best_rank') = if rank > c_best_rank then (c, rank) else (c_best, c_best_rank)
                        in case Map.lookup c g of
                            -- TODO: this Nothing is an error, should be unreachable
                            Nothing -> git_repo_get_best_bisect_commit' g ranks (c_best', c_best_rank') queue'
                            Just (_, []) ->
                                git_repo_get_best_bisect_commit' g ranks (c_best', c_best_rank') queue'
                            Just (_, ccs) ->
                                let (ranks', queue'') = calculate_final_ranks_and_queue ccs (rank+1) ranks queue'
                                in  git_repo_get_best_bisect_commit' g ranks' (c_best', c_best_rank') queue''


push_back_list_to_queue [] q = q
push_back_list_to_queue (x:xs) q = push_back_list_to_queue xs (Dequeue.pushBack q x)
