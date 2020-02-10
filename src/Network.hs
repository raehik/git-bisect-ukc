{-# LANGUAGE OverloadedStrings #-}

module Network where

import JSON
import Algo
import Data.Aeson (encode, decode)

import qualified Network.WebSockets as WS
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.UTF8 as BLU

--ws_app :: WS.ClientApp ()
ws_app user_name conn = do
    WS.sendTextData conn $ encode (JSONMsgUser user_name)
    msgRecv <- WS.receiveData conn :: IO ByteString
    case decode msgRecv :: Maybe JSONMsgProblem of
        Nothing -> print "shit"
        Just pm ->
            let pp = problem pm
            in print $ git_bisect [(good pp)] (bad pp) (dag pp)

ws_run = WS.runClient "129.12.44.229" 1234 "/" (ws_app "bo207")
