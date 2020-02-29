git_json_repo_list_to_map :: [JSONPartDagEntry] -> Map GitCommit ([GitCommit], Set GitCommit)
git_json_repo_list_to_map jl = foldl (\m (JSONPartDagEntry c c_parents) -> Map.insert c (c_parents, Set.empty) m) Map.empty jl

git_repo_show :: Map GitCommit ([GitCommit], Set GitCommit) -> String
git_repo_show g = Map.foldrWithKey (\k x ks -> ks ++ git_commit_show k x ++ "\n") "" g
git_commit_show c (c_parents, c_ancs) =
    T.unpack c
    ++ "\n  P: " ++ show c_parents
    ++ "\n  ancs: " ++ ancs_str
    where ancs_str = if Set.null c_ancs then "<uncalculated>" else show c_ancs

-- Return the relevant subgraph of a commit in the given repo. Relevant commits
-- are ones which are in c_bad's ancestry, and not in any of c_good's commits'
-- ancestry.
--
-- This is split into two phases:
--
--   * create a subgraph of the graph starting from c_bad
--     * for efficiency, we also skip adding any commits in c_good (we'd remove
--       them later, but this way we don't waste as much time on them)
--   * from that graph, remove the subgraphs of each commit in c_good
--
-- Please refer to the detailed algorithm discussion in the provided README.
git_repo_subgraph c_good c_bad g =
    let sg  = git_repo_subgraph_from_bad (Set.fromList c_good) g Map.empty [c_bad]
    in  git_repo_subgraph_remove_goods g sg c_good

-- Returns Nothing if the graph was poorly formed.
git_repo_subgraph_from_bad c_good g sg [] = Just sg
git_repo_subgraph_from_bad c_good g sg (c:cs) =
    case Map.lookup c g of
        Nothing -> Nothing
        Just (c_parents, c_ancs) ->
            let c_parents' = filter (\c -> not (Set.member c c_good)) c_parents
            in git_repo_subgraph_from_bad c_good g (Map.insert c (c_parents', c_ancs) sg) (revprepend_list c_parents' cs)

-- Returns Nothing if the graph was poorly formed.
git_repo_subgraph_remove_goods g sg [] = Just sg
git_repo_subgraph_remove_goods g sg (c:cs) =
    let Just (c_parents, c_ancs) = Map.lookup c g
    in let sg' = foldl (\g c -> Map.delete c g) sg c_parents
    in git_repo_subgraph_remove_goods g sg' (revprepend_list c_parents cs)
git_repo_subgraph_remove_goods g sg (c:cs) =
    case Map.lookup c g of
        Nothing -> Nothing
        Just (c_parents, c_ancs) ->
            let sg' = foldl (\g c -> Map.delete c g) sg c_parents
            in  git_repo_subgraph_remove_goods g sg' (revprepend_list c_parents cs)

-- Revprepend l2 onto l1.
revprepend_list l1 l2 = foldl (flip (:)) l1 l2

data AncSetOrSched
    = AncSet (Set GitCommit)
    | AncSched [GitCommit]

git_repo_get_bisect_commit_calc_limit g c_head remaining_calcs =
    git_repo_get_bisect_commit_calc_limit' g (c_head, 0) [c_head] remaining_calcs

-- Case: Whole graph calculated. Return best commit so far (== overall best).
git_repo_get_bisect_commit_calc_limit' g (c_best_current, _) [] _ = (Just c_best_current, g)

-- Case: Calculations exhausted. Return best commit so far.
git_repo_get_bisect_commit_calc_limit' g (c_best_current, _) _  0 = (Just c_best_current, g)

git_repo_get_bisect_commit_calc_limit' g (c_best_current, c_best_current_rank) (c_cur:c_stack) remaining_calcs =
    case Map.lookup c_cur g of
        -- we were given a nonexistent commit, nothing to do
        Nothing -> (Nothing, g)
        Just (c_cur_ps, c_cur_ancs) ->
            if Set.null c_cur_ancs
            then
                -- ancestors not yet calculated
                case foldl (calculate_ancestors_or_schedule_calculations g) (AncSet (Set.singleton c_cur)) c_cur_ps of
                    -- missing parent ancestors, schedule them before scheduling
                    -- this commit again
                    AncSched c_sched -> git_repo_get_bisect_commit_calc_limit' g (c_best_current, c_best_current_rank) (revprepend_list (c_cur:c_stack) c_sched) remaining_calcs
                    -- successfully calculated ancestor list
                    AncSet c_cur_ancs' ->
                        let    c_cur_rank = min (Set.size c_cur_ancs') ((Map.size g) - (Set.size c_cur_ancs'))
                        in let g' = Map.insert c_cur (c_cur_ps, c_cur_ancs') g
                        in
                            if c_cur_rank > c_best_current_rank
                            then
                                -- found a better rank: check if it's ideal
                                if c_cur_rank >= floor ((fromIntegral (Map.size g)) / 2)
                                then (Just c_cur, g')
                                else git_repo_get_bisect_commit_calc_limit' g' (c_cur, c_cur_rank) c_stack (remaining_calcs-1)
                            else git_repo_get_bisect_commit_calc_limit' g' (c_best_current, c_best_current_rank) c_stack (remaining_calcs-1)

            else
                -- ancestors already calculated
                let    c_cur_rank = min (Set.size c_cur_ancs) ((Map.size g) - (Set.size c_cur_ancs))
                in
                    if c_cur_rank > c_best_current_rank
                    then
                        -- found a better rank: check if it's ideal
                        if c_cur_rank >= floor ((fromIntegral (Map.size g)) / 2)
                        then (Just c_cur, g)
                        else git_repo_get_bisect_commit_calc_limit' g (c_cur, c_cur_rank) (revprepend_list c_cur_ps c_stack) remaining_calcs
                    else git_repo_get_bisect_commit_calc_limit' g (c_best_current, c_best_current_rank) (revprepend_list c_cur_ps c_stack) remaining_calcs

calculate_ancestors_or_schedule_calculations g (AncSet cur_ancs) c =
    case Map.lookup c g of
        Nothing -> AncSet cur_ancs
        Just (_, c_ancs) ->
            if Set.null c_ancs
            then AncSched [c]
            else AncSet (Set.union cur_ancs c_ancs)

calculate_ancestors_or_schedule_calculations g (AncSched cur_sched) c =
    case Map.lookup c g of
        Nothing -> AncSched cur_sched
        Just (_, c_ancs) ->
            if Set.null c_ancs
            then AncSched (c:cur_sched)
            else AncSched cur_sched
