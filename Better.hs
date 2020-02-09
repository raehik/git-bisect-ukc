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

git_repo_to_string g = Map.foldrWithKey (\k x ks -> ks ++ git_commit_to_string k x ++ "\n") "" g
git_commit_to_string c (cps, ccs, ancs, dscs) =
    c
    ++ "\n  P: " ++ show cps
    ++ "\n  C: " ++ show ccs
    ++ "\n  ancs: " ++ show ancs
    ++ "\n  dscs: " ++ show dscs

-- Initialise repo map (without calculating children etc. yet)
--git_repo_list_to_map :: Ord a => [(a, [a])] -> Map a ([a], [a], Int, Int)
git_repo_list_to_map l = git_repo_list_to_map' Map.empty l
git_repo_list_to_map' g [] = g
git_repo_list_to_map' g ((c, cps):cs) = git_repo_list_to_map' (Map.insert c (cps, [], 0, 0) g) cs

-- Return the subgraph of a commit in the given repo by recursing through
-- parents.
--git_repo_subgraph :: Ord a => [a] -> a -> Map a ([a], [a], Int, Int) -> Map a ([a], [a], Int, Int)
git_repo_subgraph c_good c_bad g = git_repo_subgraph' c_good g Set.empty [c_bad]
git_repo_subgraph' _ g sg_c [] = foldl (map_copy_from_map g) Map.empty sg_c
git_repo_subgraph' c_good g sg_c (c:cs) =
    case Map.lookup c g of
        Just (cps, _, _, _) -> git_repo_subgraph' c_good g (Set.insert c sg_c) (foldr (:) cps cs)
        Nothing -> git_repo_subgraph' c_good g sg_c cs

-- Copy entry k from map m1 into map m2 if it exists (else just return m2).
map_copy_from_map m1 m2 k =
    case Map.lookup k m1 of
        Just v -> Map.insert k v m2
        Nothing -> m2

-- Calculate and record children for every commit in the graph.
-- Worst case O(n^2) because we do operations for every parent of every commit.
-- Lots of confusing trickery going on here, tons of folding.
--git_repo_calculate_children :: Ord a => Map a ([a], [a], Int, Int) -> Map a ([a], [a], Int, Int)
git_repo_calculate_children g = Map.foldrWithKey git_repo_fold_children g g
git_repo_fold_children c (cps, _, _, _) m = foldl (git_repo_add_child_to_parents c) m cps
git_repo_add_child_to_parents c m cp = Map.update (git_repo_merge_children c) cp m
git_repo_merge_children cc (cps, ccs, ancs, dscs) = Just (cps, (cc:ccs), ancs, dscs)

--revprepend_list [] y = y
--revprepend_list (x:xs) y = revprepend_list xs (x:y)

--git_repo_calculate_descendants_and_get_heads :: a -> Map a ([a], [a]) -> (Map a Int, [a])
--git_repo_calculate_descendants_and_get_heads' :: Ord a => Map a ([a], [a]) -> (Map a (Int, Int), [a]) -> Dequeue.BankersDequeue a -> (Map a (Int, Int), [a])
git_repo_calculate_descendants_and_get_heads g c_bad =
    git_repo_calculate_descendants_and_get_heads' (Map.update (git_commit_set_dscs 1) c_bad g) [] (Dequeue.pushBack (Dequeue.empty :: Dequeue.BankersDequeue a) c_bad)
git_repo_calculate_descendants_and_get_heads' g heads queue =
    case Dequeue.popFront queue of
        Nothing -> (g, heads)
        Just (c, queue') ->
            case Map.lookup c g of
                -- TODO: this Nothing is an error, should be unreachable
                Nothing -> git_repo_calculate_descendants_and_get_heads' g heads queue'
                Just ([], _, _, _) ->
                    -- commit has no parents -> it's a head, note it down
                    git_repo_calculate_descendants_and_get_heads' g (c:heads) queue'
                Just (cps, _, _, dscs) ->
                    let (g', queue'') = update_ranks_and_queue (dscs+1) cps g queue'
                    in  git_repo_calculate_descendants_and_get_heads' g' heads queue''

--update_ranks_and_queue :: Ord a => [a] -> Int -> Map a Int -> Dequeue.BankersDequeue a -> (Map a Int, Dequeue.BankersDequeue a)
update_ranks_and_queue dscs [] g q = (g, q)
update_ranks_and_queue dscs (c:cs) g q =
    case Map.lookup c g of
        -- TODO: unreachable
        Nothing -> update_ranks_and_queue dscs cs g q
        Just (_, _, _, 0) -> update_ranks_and_queue dscs cs (Map.update (git_commit_set_dscs dscs) c g) (Dequeue.pushBack q c)
        otherwise -> update_ranks_and_queue dscs cs g q

-- TODO: try wrapping in Just instead of using here
git_commit_set_dscs dscs' (cps, ccs, ancs, dscs) = Just (cps, ccs, ancs, dscs')
git_commit_set_ancs ancs' (cps, ccs, ancs, dscs) = Just (cps, ccs, ancs', dscs)

-- TODO: possible speedup here, to end even earlier when calculating child ranks
-- (instead of when we actually get to that child)
-- also: fucking hell christ
git_repo_get_best_bisect_commit g cs =
    --git_repo_get_best_bisect_commit' g Nothing (push_back_list_to_queue cs (Dequeue.empty :: Dequeue.BankersDequeue a))
    git_repo_get_best_bisect_commit' (foldl (\g' c -> Map.update (git_commit_set_ancs 1) c g') g cs) Nothing (push_back_list_to_queue cs (Dequeue.empty :: Dequeue.BankersDequeue a))
git_repo_get_best_bisect_commit' g c_best queue =
    case Dequeue.popFront queue of
        Nothing ->
            case c_best of
                -- weird error, couldn't find a bisect commit (empty graph?)
                Nothing -> Nothing
                Just (c_best_commit, c_best_rank) -> Just c_best_commit
        Just (c, queue') ->
            case Map.lookup c g of
                -- TODO: this Nothing is an error, should be unreachable
                Nothing -> git_repo_get_best_bisect_commit' g c_best queue'
                Just (cps, ccs, ancs, dscs) ->
                    let
                        rank = min ancs dscs
                    in
                        let
                            (c_best_commit', c_best_rank') =
                                case c_best of
                                    Nothing -> (c, rank)
                                    Just (c_best_commit, c_best_rank) ->
                                        if rank > c_best_rank
                                        then (c, rank)
                                        else (c_best_commit, c_best_rank)
                        in
                            if c_best_rank' >= floor ((fromIntegral (Map.size g))/2)
                            then Just c_best_commit'
                            else
                                let (g', queue'') = update_ancs_and_queue (ancs+1) ccs g queue'
                                in  git_repo_get_best_bisect_commit' g' (Just (c_best_commit', c_best_rank')) queue''

--calculate_final_ranks_and_queue ccs (rank+1) ranks queue'

push_back_list_to_queue [] q = q
push_back_list_to_queue (x:xs) q = push_back_list_to_queue xs (Dequeue.pushBack q x)

update_ancs_and_queue ancs [] g q = (g, q)
update_ancs_and_queue ancs (c:cs) g q =
    case Map.lookup c g of
        -- TODO: unreachable
        Nothing -> update_ancs_and_queue ancs cs g q
        Just (_, _, 0, _) -> update_ancs_and_queue ancs cs (Map.update (git_commit_set_ancs ancs) c g) (Dequeue.pushBack q c)
        otherwise -> update_ancs_and_queue ancs cs g q
