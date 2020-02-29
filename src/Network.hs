{-# LANGUAGE OverloadedStrings #-}

module Network where

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
    c_first_bad <- client_question_loop conn [(good pp)] (bad pp) (git_json_repo_list_to_map (dag pp))
    case c_first_bad of
        Nothing -> putStrLn "could not determine first bad commit"
        Just c_first_bad' -> do
            print c_first_bad'
            WS.sendTextData conn $ encode (JSONMsgSolution c_first_bad')
            msg_score <- WS.receiveData conn :: IO ByteString
            case decode msg_score :: Maybe JSONMsgScore of
                Nothing -> putStrLn "error decoding JSON"
                Just score -> print score

client_question_loop :: WS.Connection -> [GitCommit] -> GitCommit -> Map GitCommit ([GitCommit], Set GitCommit) -> IO (Maybe GitCommit)
client_question_loop conn c_good c_bad g =
    let g' = git_repo_subgraph c_good c_bad g
    in  if Map.size g' == 1
        then return (Just c_bad)
        else
            let (c_bisect, g'') = git_repo_get_bisect_commit_calc_limit g' c_bad 100000000
            in
                case c_bisect of
                    Nothing -> return Nothing -- print "error finding bisect commit"
                    Just c_bisect' -> do
                        WS.sendTextData conn $ encode (JSONMsgQuestion c_bisect')
                        msg_answer <- WS.receiveData conn :: IO ByteString
                        case decode msg_answer :: Maybe JSONMsgAnswer of
                            Nothing -> return Nothing -- print "error decoding JSON"
                            Just msg_answer' -> do
                                let c_status = answer msg_answer'
                                putStrLn $ "query commit " ++ T.unpack c_bisect' ++ " was " ++ show c_status
                                let (c_good', c_bad') =
                                        case c_status of
                                            GitCommitGood -> ([c_bisect'], c_bad)
                                            GitCommitBad  -> (c_good, c_bisect')
                                client_question_loop conn c_good' c_bad' g''

ws_run = WS.runClient "129.12.44.229" 1234 "/" (client_init "bo207")
