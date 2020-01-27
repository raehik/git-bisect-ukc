module Main where

import Data.HashMap.Lazy as HM
import Data.HashSet as HS

main :: IO ()
main = putStrLn "Hello, Haskell!"

data Answer = GoodAnswer | BadAnswer

data Message
    = MsgUserAuth String
    | MsgProblem String GitCommitName GitCommitName GitGraph
    | MsgQuestion GitCommitName
    | MsgAnswer Answer
    | MsgSolution GitCommitName
    | MsgScore (Maybe Int)

type GitCommitName = String
type GitCommit = (GitCommitName, [GitCommitName])
type GitGraph = [GitCommit]
type GitGraphInternal = HashMap GitCommitName [GitCommitName]
data GitCommitStatus
    = GitCommitBad
    | GitCommitGood

git_nodelist_to_internal :: GitGraph -> GitGraphInternal
git_nodelist_to_internal' :: GitGraph -> GitGraphInternal -> GitGraphInternal
git_nodelist_to_internal list = git_nodelist_to_internal' list HM.empty
git_nodelist_to_internal' [] graph = graph
git_nodelist_to_internal' ((commit, parents):list) graph = git_nodelist_to_internal' list (HM.insert commit parents graph)

graph_01_ex_lin = [("a", []), ("b", ["a"]), ("c", ["b"])]
graph_02_git_keep = [
    ("b1", ["w1"]),
    ("w1", ["w2"]),
    ("x4", ["x6"]),
    ("x6", ["x7"]),
    ("x7", ["x8"]),
    ("x8", ["w7"]),
    ("x1", ["x2"]),
    ("x2", ["x3"]),
    ("x3", ["x5", "w2"]),
    ("x5", ["w3"]),
    ("w3", ["w5"]),
    ("w5", ["w6"]),
    ("w6", ["g1"]),
    ("g1", ["y1"]),
    ("y1", ["g3"]),
    ("g3", []),
    ("w2", ["w3", "w4"]),
    ("w4", ["w7"]),
    ("w7", ["g2"]),
    ("g2", ["y2", "y3"]),
    ("y2", ["y4"]),
    ("y4", []),
    ("y3", [])
    ]
graph_03_david_simple = [
    ("a", []), -- good
    ("b", ["a"]),
    ("c", ["a"]),
    ("d", ["b", "c"]),
    ("e", ["c"]),
    ("f", ["d", "e"]) -- bad
    ]

git_bisect_ro graph c_good c_bad
    | subgraph_size == 1 = c_bad
    | otherwise =
        let commit = select_bisect_commit ancestors subgraph_size Nothing (HM.keys ancestors)
        in case request_commit_status commit of
            GitCommitGood   -> git_bisect_ro graph  (HS.insert commit c_good)  c_bad
            GitCommitBad    -> git_bisect_ro graph  c_good                     commit
    where
        ancestors = filter_and_calculate_ancestors graph c_good HM.empty [c_bad]
        subgraph_size = HS.size (ancestors ! c_bad)

-- TODO
request_commit_status commit = GitCommitBad

-- TODO
select_bisect_commit ancestors graph_size (Just (commit, commit_value)) [] = commit
select_bisect_commit ancestors graph_size current_best (commit:commit_stack)
    | commit_value == (floor ((fromIntegral graph_size)/2)) = commit
    | otherwise =
        let new_best_commit = case current_best of
                Nothing -> Just (commit, commit_value)
                Just (cur_best_commit, cur_best_value) ->
                    if   commit_value > cur_best_value
                    then Just (commit, commit_value)
                    else Just (cur_best_commit, cur_best_value)
        in select_bisect_commit ancestors graph_size new_best_commit commit_stack
    where commit_num_of_ancestors = HS.size (ancestors ! commit)
          commit_value = min commit_num_of_ancestors (graph_size - commit_num_of_ancestors)

filter_and_calculate_ancestors :: GitGraphInternal -> HashSet GitCommitName -> HashMap GitCommitName (HashSet GitCommitName) -> [GitCommitName] -> HashMap GitCommitName (HashSet GitCommitName)
filter_and_calculate_ancestors graph    _       ancestors   []
    = ancestors
filter_and_calculate_ancestors graph    c_good  ancestors   (commit:stack)
    = let
        ancestors_or_schedule = ancestor_folder ancestors c_good (graph ! commit) (Left (HS.singleton commit))
      in
        case ancestors_or_schedule of
            Left calculated_ancestors -> filter_and_calculate_ancestors graph c_good (HM.insert commit calculated_ancestors ancestors) stack
            Right new_stack     -> filter_and_calculate_ancestors graph c_good ancestors (revprepend_list new_stack (commit:stack))

revprepend_list [] y = y
revprepend_list (x:xs) y = revprepend_list xs (x:y)

ancestor_folder :: HashMap GitCommitName (HashSet GitCommitName) -> HashSet GitCommitName -> [GitCommitName] -> Either (HashSet GitCommitName) [GitCommitName] -> Either (HashSet GitCommitName) [GitCommitName]
ancestor_folder ancestors c_good []                 ancestors_or_schedule
    = ancestors_or_schedule
ancestor_folder ancestors c_good (commit:commits)   ancestors_or_schedule
    | commit `elem` c_good = ancestor_folder ancestors c_good commits ancestors_or_schedule
    | otherwise =
        let
            commit_ancestors = HM.lookup commit ancestors
        in
            let
                next_ancestors_or_schedule =
                    case commit_ancestors of
                        Nothing ->
                            case ancestors_or_schedule of
                                Left _                  -> Right [commit]
                                Right to_schedule       -> Right (commit:to_schedule)
                        Just p_a ->
                            case ancestors_or_schedule of
                                Left ancestors_so_far   -> Left (HS.union p_a ancestors_so_far)
                                Right to_schedule       -> Right to_schedule
            in
                ancestor_folder ancestors c_good commits next_ancestors_or_schedule

