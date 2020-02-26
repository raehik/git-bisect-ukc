-- TODO:
--   * Map vs. HashMap (unclear) https://stackoverflow.com/questions/7894867/performant-haskell-hashed-structure

{-# LANGUAGE OverloadedStrings #-}

module Algo where

import JSON
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Dequeue as Dequeue
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
-- The subgraph holds no references to commits outside the subgraph.
--git_repo_subgraph :: Ord a => [a] -> a -> Map a ([a], [a], Int, Int) -> Map a ([a], [a], Int, Int)
git_repo_subgraph c_good c_bad g = git_repo_subgraph' (Set.fromList c_good) g Map.empty [c_bad]
git_repo_subgraph' _ g sg [] = sg
git_repo_subgraph' c_good g sg (c:cs) =
    case Map.lookup c g of
        -- TODO unreachable
        Nothing -> git_repo_subgraph' c_good g sg cs
        Just (cps, ccs, ancs, dscs) ->
            let cps' = filter (\c -> not (Set.member c c_good)) cps
            in git_repo_subgraph' c_good g (Map.insert c (cps', ccs, ancs, dscs) sg) (foldl (flip $ (:)) cps' cs)

-- Copy entry k from map m1 into map m2 if it exists (else just return m2).
map_copy_from_map m1 m2 k =
    case Map.lookup k m1 of
        Just v -> Map.insert k v m2
        Nothing -> m2

-- TODO: check I didn't fuck up here (likely)
-- Calculate and record children for every commit in the graph.
-- Worst case O(n^2) because we do operations for every parent of every commit.
-- Lots of confusing trickery going on here, tons of folding.
--git_repo_calculate_children :: Ord a => Map a ([a], [a], Int, Int) -> Map a ([a], [a], Int, Int)
git_repo_calculate_children g = Map.foldrWithKey git_repo_fold_children g g
git_repo_fold_children c (cps, _, _, _) m = foldl (git_repo_add_child_to_parents c) m cps
git_repo_add_child_to_parents c m cp = Map.update (git_repo_merge_children c) cp m
git_repo_merge_children cc (cps, ccs, ancs, dscs) = Just (cps, (cc:ccs), ancs, dscs)

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

update_ancs_and_queue ancs [] g q = (g, q)
update_ancs_and_queue ancs (c:cs) g q =
    case Map.lookup c g of
        -- TODO: unreachable
        Nothing -> update_ancs_and_queue ancs cs g q
        Just (_, _, 0, _) -> update_ancs_and_queue ancs cs (Map.update (git_commit_set_ancs ancs) c g) (Dequeue.pushBack q c)
        otherwise -> update_ancs_and_queue ancs cs g q

push_back_list_to_queue [] q = q
push_back_list_to_queue (x:xs) q = push_back_list_to_queue xs (Dequeue.pushBack q x)

--request_git_commit_status :: GitCommit -> IO GitCommitStatus
request_git_commit_status c = do
    T.putStr $ T.concat ["Is commit ", c, " good or bad? (g/b) > "]
    input <- getLine
    case input of
        "g" -> return GitCommitGood
        "b" -> return GitCommitBad
        _ -> request_git_commit_status c

git_bisect c_good c_bad l =
    let first_graph = git_repo_calculate_children $ git_repo_subgraph c_good c_bad l
    in  git_bisect' c_good c_bad first_graph

--git_bisect' :: [a] -> a -> Map a ([a], [a], Int, Int) -> Maybe a
git_bisect' c_good c_bad g =
    let g' = git_repo_subgraph c_good c_bad g
    in  if Map.size g' == 1
        then return (Just c_bad)
        else
            let    (g'', heads) = git_repo_calculate_descendants_and_get_heads g' c_bad
            in let c_bisect =     git_repo_get_best_bisect_commit g'' heads
            in
                case c_bisect of
                    Nothing -> return Nothing
                    Just c -> do
                        c_status <- request_git_commit_status c
                        case c_status of
                            GitCommitGood -> git_bisect' (c:c_good) c_bad g'
                            GitCommitBad -> git_bisect' c_good c g'

git_select_bisect_commit c_good c_bad g =
    let g' = git_repo_subgraph c_good c_bad g
    in  if Map.size g' == 1
        then Just c_bad
        else
            let    (g'', heads) = git_repo_calculate_descendants_and_get_heads g' c_bad
            in git_repo_get_best_bisect_commit g'' heads

-- git_json_repo_list_to_map :: (Num c, Num d) => [JSONPartDagEntry] -> Map GitCommit ([GitCommit], [GitCommit], c, d)
git_json_repo_list_to_map jl = git_json_repo_list_to_map' Map.empty jl
git_json_repo_list_to_map' g [] = g
git_json_repo_list_to_map' g ((JSONPartDagEntry c cps):jps) = git_json_repo_list_to_map' (Map.insert c (cps, [], 0, 0) g) jps

git_repo_get_bisect_commit_calc_limit g c_head remaining_calcs =
    git_repo_get_bisect_commit_calc_limit' g c_head (c_head, 0) [c_head] remaining_calcs

-- Case: Whole graph calculated. Return best commit so far (== overall best).
git_repo_get_bisect_commit_calc_limit' _ _ (c_best_current, _) [] _ = c_best_current

-- Case: Calculations exhausted. Return best commit so far.
git_repo_get_bisect_commit_calc_limit' _ _ (c_best_current, _) _  0 = c_best_current

git_repo_get_bisect_commit_calc_limit' g c_head (c_best_current, c_best_current_rank) (c_cur:c_stack) remaining_calcs =
    case Map.lookup c_cur g of
        -- Nothing should be an error
        Nothing -> git_repo_get_bisect_commit_calc_limit' g c_head (c_best_current, c_best_current_rank) c_stack remaining_calcs
        Just (cps, _, _, _) ->
            
