{-# LANGUAGE OverloadedStrings #-}

module Network where

import JSON
import Algo
import Data.Aeson (encode, decode)

import qualified Network.WebSockets as WS
import Data.ByteString.Lazy (ByteString)
import qualified Data.Text as T
import qualified Data.Text.IO as T

--ws_app :: WS.ClientApp ()
ws_app user_name conn = do
    T.putStrLn "potentially connected"
    T.putStrLn "sending user message..."
    WS.sendTextData conn $ encode (JSONMsgUser user_name)
    T.putStrLn "receiving problem message..."
    msgRecv <- WS.receiveData conn :: IO ByteString
    T.putStrLn "decoding JSON..."
    case decode msgRecv :: Maybe JSONMsgProblem of
        Nothing -> T.putStrLn "error decoding JSON"
        Just pm ->
            let    pp = problem pm
            in let g  = git_repo_subgraph [(good pp)] (bad pp) (git_json_repo_list_to_map (dag pp))
            in
                T.putStrLn "selecting bisect commit..." >>
                case git_repo_get_bisect_commit_calc_limit g (bad pp) 1 of
                    Nothing -> T.putStrLn "error finding bisect commit"
                    Just c -> T.putStrLn c

ws_run = WS.runClient "129.12.44.229" 1234 "/" (ws_app "bo207")
