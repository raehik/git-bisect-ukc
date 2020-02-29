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
    cancs <- git_graph_entry_ancestors gge
    git_repo_remove_ancestors_of cgs (map_delete_list (Set.toList cancs) g)

-- Subgraph on c and invalidate ancestors.
-- (Filter good step 2.)
git_repo_subgraph_invalidate_ancestors c g =
    git_repo_dfs_map (\gge -> GitGraphEntry (git_graph_entry_parents gge) Nothing) g c

-- Subgraph on c.
-- (Filter bad.)
git_repo_subgraph c g = git_repo_dfs_map id g c

revprepend_list :: Foldable t => [a] -> t a -> [a]
revprepend_list = foldl (flip (:))

map_delete_list :: (Foldable t, Ord k) => t k -> Map k a -> Map k a
map_delete_list = flip $ foldl (flip Map.delete)

-- Apply a function to each GitCommit in a GitGraph, traversing via depth-first
-- search.
--
-- Returns Nothing if the subgraph part of the graph is invalid (e.g. commits
-- referenced as parents but not present)
git_repo_dfs_map :: (GitGraphEntry -> GitGraphEntry) -> GitGraph -> GitCommit -> Maybe GitGraph
git_repo_dfs_map f g head = do
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
