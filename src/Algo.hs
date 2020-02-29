{-# LANGUAGE OverloadedStrings #-}

module Algo where

import Data
import JSON
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

type GitGraph = Map GitCommit GitGraphEntry
data GitGraphEntry = GitGraphEntry {
    git_graph_entry_parents :: [GitCommit],
    git_graph_entry_ancestors :: Maybe (Set GitCommit)
} deriving (Show)

-- Remove ancestors of the provided commits from the provided graph.
--
-- Returns Nothing if the graph was invalid, or if the commits to remove did not
-- have their ancestors calculated. (In a normal algorithm run, due to how the
-- algorithm works, they will.)
--
-- (Filter good step 1.)
git_repo_remove_ancestors_of :: [GitCommit] -> GitGraph -> Maybe GitGraph
git_repo_remove_ancestors_of [] g = Just g
git_repo_remove_ancestors_of (cg:cgs) g = do
    gge <- Map.lookup cg g
    c_ancs <- git_graph_entry_ancestors gge
    git_repo_remove_ancestors_of cgs (map_delete_list (Set.toList c_ancs) g)

-- Subgraph on c and invalidate ancestors.
-- (Filter good step 2.)
git_repo_subgraph_invalidate_ancestors :: GitCommit -> GitGraph -> Maybe GitGraph
git_repo_subgraph_invalidate_ancestors =
    git_repo_dfs_map $ \gge -> GitGraphEntry (git_graph_entry_parents gge) Nothing

-- Subgraph on c.
-- (Filter bad.)
git_repo_subgraph :: GitCommit -> GitGraph -> Maybe GitGraph
git_repo_subgraph = git_repo_dfs_map id

revprepend_list :: Foldable t => [a] -> t a -> [a]
revprepend_list = foldl (flip (:))

map_delete_list :: (Foldable t, Ord k) => t k -> Map k a -> Map k a
map_delete_list = flip $ foldl (flip Map.delete)

-- Apply a function to each GitCommit in a GitGraph, traversing via depth-first
-- search.
--
-- Returns Nothing if the subgraph part of the graph is invalid (e.g. commits
-- referenced as parents but not present)
git_repo_dfs_map :: (GitGraphEntry -> GitGraphEntry) -> GitCommit -> GitGraph -> Maybe GitGraph
git_repo_dfs_map f head g = do
    gge <- Map.lookup head g
    dfs [head] (Map.singleton head (f gge))
    where
        dfs [] sg = Just sg
        dfs (c:cs) sg = do
            cv <- Map.lookup c sg
            (sg', cs') <- dfs_add_unseen cs sg (git_graph_entry_parents cv)
            dfs cs' sg'
        dfs_add_unseen cs sg [] = Just (sg, cs)
        dfs_add_unseen cs sg (cp:cps) = do
            (sg', cs') <-
                if Map.notMember cp sg then
                    Map.lookup cp g >>=
                        (\gge -> Just (Map.insert cp (f gge) sg, (cp:cs)))
                else Just (sg, cs)
            dfs_add_unseen cs' sg' cps

-- Convert a repo in JSON representation to a GitGraph.
-- Makes no validity checks, and does not calculate ancestors.
git_json_repo_to_graph :: [JSONPartDagEntry] -> GitGraph
git_json_repo_to_graph = foldl (\g (JSONPartDagEntry c cps) -> Map.insert c (GitGraphEntry cps Nothing) g) Map.empty

git_repo_select_bisect_with_limit :: Integer -> GitCommit -> GitGraph -> Maybe (GitCommit, GitGraph)
git_repo_select_bisect_with_limit remaining_calcs head g =
    git_repo_select_bisect_with_limit' remaining_calcs [head] g (Set.singleton head) (head, 0)
git_repo_select_bisect_with_limit' remaining_calcs c_stack g c_checked (c_best_cur, c_best_cur_rank) = Nothing

-- Case: Graph traversal ended. Return best commit so far (== overall best).
git_repo_select_bisect_with_limit' _ [] g _ (c_best_cur, _) = Just (c_best_cur, g)

-- Case: Calculations exhausted. Return best commit so far.
git_repo_select_bisect_with_limit' 0 _  g _ (c_best_cur, _) = Just (c_best_cur, g)

git_repo_select_bisect_with_limit' remaining_calcs (c_cur:c_stack) g c_checked (c_best_cur, c_best_cur_rank) =
    if Set.member c_cur c_checked then
        git_repo_select_bisect_with_limit' remaining_calcs c_stack g c_checked (c_best_cur, c_best_cur_rank)
    else do
        (cps, c_ancs) <- Map.lookup c_cur g
        case c_ancs of
            Nothing ->
                NEED_CALC
            Just c_ancs' ->
                let    c_rank = min (Set.size c_ancs') ((Map.size g) - (Set.size c_ancs')
                in let graph_size_half = fromIntegral (Map.size g) / 2
                in let (c_best_cur', c_best_cur_rank') =
                        if c_rank > c_best_cur_rank
                        then (c_cur, c_rank)
                        else (c_best_cur, c_best_cur_rank)
                in let c_stack' =
                        if c_rank > ceiling graph_size_half
                        then revprepend_list cps c_stack
                        else c_stack
                in
                    if c_rank > c_best_cur_rank then
                        if c_rank >= floor graph_size_half then
                            Just (c_cur, g)
                        else git_repo_select_bisect_with_limit' remaining_calcs c_stack' g c_checked (c_best_cur', c_best_cur_rank')
                    else git_repo_select_bisect_with_limit' remaining_calcs c_stack' g c_checked (c_best_cur', c_best_cur_rank')
