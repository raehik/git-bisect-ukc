{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitBisect.Network.Client where

import GitBisect.Types
import qualified GitBisect.Network.Messages as Msg
import qualified GitBisect.Algo as Algo

import qualified Data.Aeson as Aeson
import qualified Network.WebSockets as WS
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Data.Either.Combinators (mapLeft)
import Control.Error.Safe (tryRight, tryJust)
import Data.Maybe (fromMaybe)

data ClientConfig = ClientConfig {
    clientConfigAuth :: ClientAuth
} deriving (Show)

data ClientAuth = ClientAuth {
    clientAuthUser :: Text,
    clientAuthToken :: Text
} deriving (Show)

data ServerConfig = ServerConfig {
    serverConfigHost :: String,
    serverConfigPort :: Int,
    serverConfigPath :: String
} deriving (Show)

data NetError
    = NetErrorUnspecified
    | NetErrorEncounteredMsgErrorDuringDecode Msg.MsgError
    | NetErrorUnimplemented
    | NetErrorEncounteredAlgoErrorDuringSubgraph
    | NetErrorEncounteredAlgoErrorDuringBisectSelection
    deriving (Show)

type ClientResult = Either NetError String
type Client = WS.Connection -> IO ClientResult

-- Initialise repo map with empty ancestors.
git_repo_list_to_map :: [(Msg.MsgString, [Msg.MsgString])] -> GitGraph
git_repo_list_to_map l = foldl (\m (c, c_parents) -> Map.insert c (GitGraphEntry c_parents Nothing) m) Map.empty l

send :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
send conn msg = do
    WS.sendTextData conn $ Aeson.encode msg
    putStrLn "message sent"

recv :: WS.WebSocketsData a => WS.Connection -> IO a
recv conn = do
    d <- WS.receiveData conn
    putStrLn "message received"
    return d

serverCfg_test = ServerConfig "129.12.44.229" 1234 "/"
clientCfg_bo207 = ClientConfig $ ClientAuth "bo207" "49ea39ac"

client_bo207 = client clientCfg_bo207

-- | Run a client against the given server.
run :: Client -> ServerConfig -> IO ClientResult
run c sc = do
    putStrLn "starting client..."
    WS.runClient (serverConfigHost sc) (serverConfigPort sc) (serverConfigPath sc) c

showClientResult (Left err) = "nope sry, error: " ++ show err
showClientResult (Right yay) = "yay worked, msg: " ++ yay

decodeOrWrapError msg = mapLeft NetErrorEncounteredMsgErrorDuringDecode $ Msg.decode msg

tryRecvAndDecode :: Aeson.FromJSON a => WS.Connection -> ExceptT NetError IO a
tryRecvAndDecode conn = do
    msg <- lift $ (recv conn :: IO ByteString)
    tryRight $ decodeOrWrapError msg

filter_both cGood cBad g =
    Algo.git_repo_sg_bad cBad g >>= Algo.git_repo_sg_good cGood cBad

filter_good = Algo.git_repo_sg_good
filter_bad = Algo.git_repo_sg_bad

-- all done in an ExceptT ClientResult IO a
-- lift wraps an IO a into our monad
-- return wraps an a into our monad
client :: ClientConfig -> WS.Connection -> IO ClientResult
client cc conn = runExceptT $ do
    lift $ putStrLn "client started"

    -- Authenticate with server
    let cca = clientConfigAuth cc
    lift $ send conn $ Msg.MAuth (clientAuthUser cca) (clientAuthToken cca)
    lift $ putStrLn "authenticated"

    -- Receive a repo
    mRepo :: Msg.MRepo <- tryRecvAndDecode conn
    let g = git_repo_list_to_map (Msg.mRepoDag mRepo)
    lift $ putStrLn "received repo"

    -- Receive an instance
    mInstance :: Msg.MInstance <- tryRecvAndDecode conn
    let cGood = Msg.mInstanceGood mInstance
    let cBad = Msg.mInstanceBad mInstance
    lift $ putStrLn "received instance"

    -- Perform special good+bad initial filter
    sg <- tryJust NetErrorEncounteredAlgoErrorDuringSubgraph $ filter_both cGood cBad g
    lift $ putStrLn "filtered graph both sides"

    cAnswer <- clientStateBisectLoop conn cGood cBad sg

    lift $ print $ cAnswer
    ExceptT $ return $ Left $ NetErrorUnimplemented

clientStateBisectLoop :: WS.Connection -> GitCommit -> GitCommit -> GitGraph -> ExceptT NetError IO GitCommit
clientStateBisectLoop conn cGood cBad g =
    if Map.size g == 1 then ExceptT $ return $ Right $ cBad
    else do
        lift $ print $ Map.size g
        (cBisect, g') <- tryJust NetErrorEncounteredAlgoErrorDuringBisectSelection $ Algo.git_repo_select_bisect_with_limit 10000 cBad g
        lift $ send conn $ Msg.MQuestion cBisect
        mAnswer :: Msg.MAnswer <- tryRecvAndDecode conn
        case (Msg.mAnswerCommitStatus mAnswer) of
            Msg.CommitGood -> do
                g'' <- tryJust NetErrorEncounteredAlgoErrorDuringSubgraph $ filter_good cBisect cBad g'
                clientStateBisectLoop conn cGood cBisect g''
            Msg.CommitBad -> do
                g'' <- tryJust NetErrorEncounteredAlgoErrorDuringSubgraph $ filter_bad cBisect g'
                clientStateBisectLoop conn cGood cBisect g''
