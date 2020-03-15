{-# LANGUAGE OverloadedStrings #-}

module GitBisect.AlgoOld where

import GitBisect.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Foldable

-- Also invalidates ancestors.
f_lookup_fix c g = do
    CommitGraphEntry p _ <- Map.lookup c g
    Just $ CommitGraphEntry (filter (flip Map.member g) p) Nothing

f_fold_del g c (CommitGraphEntry p Nothing) = (Map.delete c g, [])
f_fold_del g c (CommitGraphEntry p (Just ancs)) = (foldl (flip Map.delete) g ancs, p)
f_fold_sub g c gge = (Map.insert c gge g, [])

git_repo_sg_good c_g c_b g = do
    git_repo_dfs_foldl Map.lookup f_fold_del g c_g g >>=
        git_repo_dfs_foldl f_lookup_fix f_fold_sub Map.empty c_b

git_repo_sg_bad c g = git_repo_dfs_foldl Map.lookup (\g c gge -> (Map.insert c gge g, [])) Map.empty c g

--dfs_step :: Set CommitID -> [CommitID] -> CommitGraph -> Maybe ([CommitID], Set CommitID)
--dfs_step c_seen [] g = Just ([], c_seen)
--dfs_step c_seen (c:cs) g = do
--    CommitGraphEntry p ancs <- Map.lookup c g
--    foldl dfs_step_add_parents (g, c_seen) p
--    where
--        dfs_step_add_parents g 

-- Remove ancestors of the provided commits from the provided graph.
--
-- Returns Nothing if the graph was invalid, or if the commits to remove did not
-- have their ancestors calculated. (In a normal algorithm run, due to how the
-- algorithm works, they will.)
--
-- (Filter good step 1.)
git_repo_remove_ancestors_of :: [CommitID] -> CommitGraph -> Maybe CommitGraph
git_repo_remove_ancestors_of [] g = Just g
git_repo_remove_ancestors_of (cg:cgs) g = do
    gge <- Map.lookup cg g
    c_ancs <- commitGraphEntryAncestors gge
    git_repo_remove_ancestors_of cgs (map_delete_list (Set.toList c_ancs) g)

-- TODO: need to generalise remove to do a DFS where ancestors aren't calculated

-- Subgraph on c and invalidate ancestors.
-- (Filter good step 2.)
git_repo_subgraph_invalidate_ancestors :: CommitID -> CommitGraph -> Maybe CommitGraph
git_repo_subgraph_invalidate_ancestors =
    git_repo_dfs_map $ \gge -> CommitGraphEntry (commitGraphEntryParents gge) Nothing

-- Subgraph on c.
-- (Filter bad.)
git_repo_subgraph :: CommitID -> CommitGraph -> Maybe CommitGraph
git_repo_subgraph = git_repo_dfs_map id

revprepend_list :: Foldable t => [a] -> t a -> [a]
revprepend_list = foldl (flip (:))

map_delete_list :: (Foldable t, Ord k) => t k -> Map k a -> Map k a
map_delete_list = flip $ foldl (flip Map.delete)

git_repo_dfs_foldl :: (CommitID -> CommitGraph -> Maybe CommitGraphEntry) -> (CommitGraph -> CommitID -> CommitGraphEntry -> (CommitGraph, [CommitID])) -> CommitGraph -> CommitID -> CommitGraph -> Maybe CommitGraph
git_repo_dfs_foldl f_lookup f_fold g_fold head g =
    dfs g_fold [head] (Set.singleton head)
    where
        dfs g_fold [] c_seen = Just g_fold
        dfs g_fold (c:cs) c_seen = do
            cv <- f_lookup c g
            let (g_fold', c_skip) = f_fold g_fold c cv
            let c_seen' = foldl (flip Set.insert) c_seen c_skip
            let (cs', c_seen'') = foldl dfs_add_unseen (cs, c_seen') (commitGraphEntryParents cv)
            dfs g_fold' cs' c_seen''

        dfs_add_unseen (cs, c_seen) c =
            if Set.notMember c c_seen
            then (c:cs, Set.insert c c_seen)
            else (cs, c_seen)

-- Apply a function to each commit in a graph, traversing via depth-first
-- search.
--
-- Returns Nothing if the subgraph part of the graph is invalid (e.g. commits
-- referenced as parents but not present)
git_repo_dfs_map :: (CommitGraphEntry -> CommitGraphEntry) -> CommitID -> CommitGraph -> Maybe CommitGraph
git_repo_dfs_map f head g = do
    gge <- Map.lookup head g
    dfs [head] (Map.singleton head (f gge))
    where
        dfs [] sg = Just sg
        dfs (c:cs) sg = do
            cv <- Map.lookup c sg
            (sg', cs') <- dfs_add_unseen cs sg (commitGraphEntryParents cv)
            dfs cs' sg'
        dfs_add_unseen cs sg [] = Just (sg, cs)
        dfs_add_unseen cs sg (cp:cps) = do
            (sg', cs') <-
                if Map.notMember cp sg then
                    Map.lookup cp g >>=
                        (\gge -> Just ((Map.insert cp (f gge) sg), (cp:cs)))
                else Just (sg, cs)
            dfs_add_unseen cs' sg' cps

git_repo_select_bisect_with_limit :: Integer -> CommitID -> CommitGraph -> Maybe (CommitID, CommitGraph)
git_repo_select_bisect_with_limit rem_calcs head g =
    git_repo_select_bisect_with_limit' rem_calcs [head] g (Set.singleton head) (head, 0)

-- Case: Graph traversal ended. Return best commit so far (== overall best).
git_repo_select_bisect_with_limit' _ [] g _ (c_best_cur, _) = Just (c_best_cur, g)

-- Case: Calculations exhausted. Return best commit so far.
git_repo_select_bisect_with_limit' 0 _  g _ (c_best_cur, _) = Just (c_best_cur, g)

git_repo_select_bisect_with_limit' rem_calcs (c_cur:c_stack) g c_checked (c_best_cur, c_best_cur_rank) =
    if Set.member c_cur c_checked then
        -- already calculated+checked, skip
        git_repo_select_bisect_with_limit' rem_calcs c_stack g c_checked (c_best_cur, c_best_cur_rank)
    else do
        gge <- Map.lookup c_cur g
        let cps = commitGraphEntryParents gge
        case commitGraphEntryAncestors gge of
            Nothing -> do
                -- ancestors not yet calculated
                ancs_or_sched <- foldlM (calculate_ancs_or_sched g) (AncSet (Set.singleton c_cur)) cps
                case ancs_or_sched of
                    -- missing some parent ancestors, schedule them then
                    -- reschedule current commit
                    AncSched c_sched ->
                        let c_stack' = revprepend_list (c_cur:c_stack) c_sched
                        in  git_repo_select_bisect_with_limit' rem_calcs c_stack' g c_checked (c_best_cur, c_best_cur_rank)
                    -- successfully calculated ancestors: reschedule ourselves,
                    -- we check in the other case (shouldn't be a big slowdown,
                    -- keeps code much neater)
                    AncSet c_ancs ->
                        let g' = Map.insert c_cur (CommitGraphEntry cps (Just c_ancs)) g
                        in  git_repo_select_bisect_with_limit' rem_calcs (c_cur:c_stack) g' c_checked (c_best_cur, c_best_cur_rank)
            Just c_ancs ->
                -- ancestors already calculated
                let    c_checked' = Set.insert c_cur c_checked
                in let rem_calcs' = rem_calcs-1
                in let c_rank = min (Set.size c_ancs) ((Map.size g) - (Set.size c_ancs))
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
                        -- better rank found
                        if c_rank >= floor graph_size_half then
                            -- it's an ideal bisect commit: end early
                            Just (c_cur, g)
                        else git_repo_select_bisect_with_limit' rem_calcs' c_stack' g c_checked' (c_best_cur', c_best_cur_rank')
                    else git_repo_select_bisect_with_limit' rem_calcs' c_stack' g c_checked' (c_best_cur', c_best_cur_rank')

data EitherAncSetOrSched
    = AncSet (Set CommitID)
    | AncSched [CommitID]

calculate_ancs_or_sched g (AncSet cur_ancs) c = do
    gge <- Map.lookup c g
    case commitGraphEntryAncestors gge of
        Nothing -> Just $ AncSched [c]
        Just c_ancs -> Just $ AncSet (Set.union c_ancs cur_ancs)

calculate_ancs_or_sched g (AncSched cur_sched) c = do
    gge <- Map.lookup c g
    case commitGraphEntryAncestors gge of
        Nothing -> Just $ AncSched (c:cur_sched)
        Just c_ancs -> Just $ AncSched cur_sched
