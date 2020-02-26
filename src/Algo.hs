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

git_json_repo_list_to_map :: [JSONPartDagEntry] -> Map GitCommit ([GitCommit], Set GitCommit)
git_json_repo_list_to_map jl = foldl (\m (JSONPartDagEntry c c_parents) -> Map.insert c (c_parents, Set.empty) m) Map.empty jl

git_repo_show :: Map GitCommit ([GitCommit], Set GitCommit) -> String
git_repo_show g = Map.foldrWithKey (\k x ks -> ks ++ git_commit_show k x ++ "\n") "" g
git_commit_show c (c_parents, c_ancs) =
    T.unpack c
    ++ "\n  P: " ++ show c_parents
    ++ "\n  ancs: " ++ show ancs_str
    where ancs_str = if Set.null c_ancs then "<uncalculated>" else show c_ancs

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
        Just (cps, ancs) ->
            let cps' = filter (\c -> not (Set.member c c_good)) cps
            in git_repo_subgraph' c_good g (Map.insert c (cps', ancs) sg) (revprepend_list cps' cs)

-- Revprepend l2 onto l1.
revprepend_list l1 l2 = foldl (flip (:)) l1 l2

data AncSetOrSched
    = AncSet (Set GitCommit)
    | AncSched [GitCommit]

git_repo_get_bisect_commit_calc_limit g c_head remaining_calcs =
    git_repo_get_bisect_commit_calc_limit' g (c_head, 0) [c_head] remaining_calcs

-- Case: Whole graph calculated. Return best commit so far (== overall best).
git_repo_get_bisect_commit_calc_limit' _ (c_best_current, _) [] _ = Just c_best_current

-- Case: Calculations exhausted. Return best commit so far.
git_repo_get_bisect_commit_calc_limit' _ (c_best_current, _) _  0 = Just c_best_current

git_repo_get_bisect_commit_calc_limit' g (c_best_current, c_best_current_rank) (c_cur:c_stack) remaining_calcs =
    case Map.lookup c_cur g of
        Nothing -> Nothing
        Just (c_cur_ps, _) ->
            case foldl (calculate_ancestors_or_schedule_calculations g) (AncSet (Set.singleton c_cur)) c_cur_ps of
                AncSched c_sched -> git_repo_get_bisect_commit_calc_limit' g (c_best_current, c_best_current_rank) (revprepend_list (c_cur:c_stack) c_sched) remaining_calcs
                AncSet c_cur_ancs ->
                    let c_cur_rank = min (Set.size c_cur_ancs) ((Map.size g) - (Set.size c_cur_ancs))
                    in
                        if c_cur_rank >= floor (Map.size g)/2)
                        then Just c_cur
                        else
                            in let (c_best_current', c_best_current_rank') = if c_cur_rank > c_best_current_rank then (c_cur, c_cur_rank) else (c_best_current, c_best_current_rank)
                            in let g' = Map.insert c_cur (c_cur_ps, c_cur_ancs) g
                            in     git_repo_get_bisect_commit_calc_limit' g' (c_best_current', c_best_current_rank') c_stack (remaining_calcs-1)

calculate_ancestors_or_schedule_calculations g (AncSet cur_ancs) c =
    case Map.lookup c g of
        -- Nothing should probably be an error
        Nothing -> AncSet cur_ancs
        Just (_, c_ancs) ->
            if Set.null c_ancs
            then AncSched [c]
            else AncSet (Set.union cur_ancs c_ancs)

calculate_ancestors_or_schedule_calculations g (AncSched cur_sched) c =
    case Map.lookup c g of
        -- Nothing should probably be an error
        Nothing -> AncSched cur_sched
        Just (_, c_ancs) ->
            if Set.null c_ancs
            then AncSched (c:cur_sched)
            else AncSched cur_sched
