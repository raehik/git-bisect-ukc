module Main where

import Data.HashMap.Lazy as HM

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
type GitGraphInternal = HashMap GitCommitName ([GitCommitName], Int)

revprepend_list [] y = y
revprepend_list (x1:xs) y = revprepend_list xs (x1:y)

git_nodelist_to_internal :: GitGraph -> GitGraphInternal
git_nodelist_to_internal' :: GitGraph -> GitGraphInternal -> GitGraphInternal
git_nodelist_to_internal list = git_nodelist_to_internal' list HM.empty
git_nodelist_to_internal' [] graph = graph
git_nodelist_to_internal' ((commit, parents):list) graph = git_nodelist_to_internal' list (HM.insert commit (parents, 0) graph)

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

select_from_hashmap graph wanted_keys = select_from_hashmap' graph wanted_keys HM.empty
select_from_hashmap' _ [] filtered_graph = filtered_graph
select_from_hashmap' graph (wanted_key:wanted_keys) filtered_graph =
    select_from_hashmap' graph wanted_keys (HM.insert wanted_key (graph ! wanted_key) filtered_graph)

filter_graph :: [GitCommitName] -> GitCommitName -> GitGraphInternal -> GitGraphInternal
filter_graph' :: [GitCommitName] -> GitGraphInternal -> [GitCommitName] -> [GitCommitName] -> GitGraphInternal
filter_graph c_good c_bad graph =
    filter_graph' c_good graph [c_bad] (case HM.lookup c_bad graph of Nothing -> []; Just (parents, _) -> parents)
filter_graph' _ graph c_selected [] =
    select_from_hashmap graph c_selected
filter_graph' c_good graph c_selected (c_current:c_workstack)
    | c_current `elem` c_good =
        filter_graph' c_good graph c_selected c_workstack
    | otherwise =
        case HM.lookup c_current graph of
            Nothing -> HM.empty -- TODO malformed graph
            Just ([], _) -> filter_graph' c_good graph (c_current:c_selected) c_workstack
            Just (parents, _) -> filter_graph' c_good graph (c_current:c_selected) (revprepend_list parents c_workstack)

--select_bisect_commit :: GitGraphInternal -> Int -> [GitCommitName] -> GitCommitName
--select_bisect_commit graph size stack

-- git_bisect [c_good] c_bad graph
--git_bisect :: [GitCommitName] -> GitCommitName -> GitGraphInternal -> GitCommitName
