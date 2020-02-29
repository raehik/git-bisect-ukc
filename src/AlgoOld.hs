git_json_repo_list_to_map :: [JSONPartDagEntry] -> Map GitCommit ([GitCommit], Set GitCommit)
git_json_repo_list_to_map jl = foldl (\m (JSONPartDagEntry c c_parents) -> Map.insert c (c_parents, Set.empty) m) Map.empty jl

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
