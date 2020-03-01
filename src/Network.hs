{-# LANGUAGE OverloadedStrings #-}

module Network where

import Data
import JSON
import Algo

import Data.Aeson (encode, decode)
import qualified Network.WebSockets as WS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

client_init :: String -> WS.ClientApp ()
client_init user_name conn = do
    putStrLn "potentially connected"
    putStrLn "sending user message..."
    WS.sendTextData conn $ encode (JSONMsgUser (T.pack user_name))
    putStrLn "receiving problem message..."
    msg_prob <- WS.receiveData conn :: IO ByteString
    putStrLn "decoding JSON..."
    case decode msg_prob :: Maybe JSONMsgProblem of
        Nothing -> putStrLn "error decoding JSON"
        Just msg_prob' -> client_solve_problem conn msg_prob'

client_solve_problem conn prob =
    let pp = problem prob in do
    c_first_bad <- client_question_loop conn [(good pp)] (bad pp) (git_json_repo_to_graph (dag pp))
    case c_first_bad of
        Nothing -> putStrLn "could not determine first bad commit"
        Just c_first_bad' -> do
            print c_first_bad'
            WS.sendTextData conn $ encode (JSONMsgSolution c_first_bad')
            msg_score <- WS.receiveData conn :: IO ByteString
            case decode msg_score :: Maybe JSONMsgScore of
                Nothing -> putStrLn "error decoding JSON"
                Just score -> print score

client_question_loop :: WS.Connection -> [GitCommit] -> GitCommit -> GitGraph -> IO (Maybe GitCommit)
client_question_loop conn c_good c_bad g
    | Map.size g == 1 = return (Just c_bad)
    | otherwise = do
        print $ Map.size g
        case git_repo_select_bisect_with_limit 100000000 c_bad g of
            Nothing -> return Nothing -- bad graph
            Just (c_bisect, g') -> do
                WS.sendTextData conn $ encode (JSONMsgQuestion c_bisect)
                msg_answer <- WS.receiveData conn :: IO ByteString
                case decode msg_answer :: Maybe JSONMsgAnswer of
                    Nothing -> return Nothing -- fucked JSON
                    Just c_status ->
                        case filter_graph c_good c_bad g' c_bisect (answer c_status) of
                            Nothing -> return Nothing -- fucked graph
                            Just (c_good', c_bad', g'') -> client_question_loop conn c_good' c_bad' g''

filter_graph :: [GitCommit] -> GitCommit -> GitGraph -> GitCommit -> GitCommitStatus -> Maybe ([GitCommit], GitCommit, GitGraph)
filter_graph c_good c_bad g c_bisect GitCommitGood = do
    g' <- git_repo_remove_ancestors_of [c_bisect] g >>= git_repo_subgraph_invalidate_ancestors c_bad
    Just ([c_bisect], c_bad, g')
filter_graph c_good c_bad g c_bisect GitCommitBad = do
    g' <- git_repo_subgraph c_bisect g
    Just (c_good, c_bisect, g')

ws_run = WS.runClient "129.12.44.229" 1234 "/" (client_init "bo207")
