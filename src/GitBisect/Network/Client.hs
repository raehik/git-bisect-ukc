{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitBisect.Network.Client where

import GitBisect.Types
import qualified GitBisect.Network.Messages as Msg
import qualified GitBisect.Algo as Algo
import qualified GitBisect.AlgoOld as AlgoOld

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

data Error
    = ErrorUnspecified
    | ErrorEncounteredMsgErrorDuringDecode Msg.MsgError
    | ErrorUnimplemented
    | ErrorEncounteredAlgoErrorDuringSubgraph Algo.Error
    | ErrorEncounteredAlgoErrorDuringBisectSelection
    | ErrorServerError
    deriving (Show)

type ClientResult = Either Error String
type Client = WS.Connection -> IO ClientResult

-- Initialise repo map with empty ancestors.
git_repo_list_to_map :: [(Msg.MsgString, [Msg.MsgString])] -> GitGraph
git_repo_list_to_map l = foldl (\m (c, c_parents) -> Map.insert c (GitGraphEntry c_parents Nothing) m) Map.empty l

send :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
send conn msg = do
    WS.sendTextData conn $ Aeson.encode msg
    --putStrLn "message sent"

recv :: WS.WebSocketsData a => WS.Connection -> IO a
recv conn = do
    d <- WS.receiveData conn
    --putStrLn "message received"
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

decodeOrWrapError msg = mapLeft ErrorEncounteredMsgErrorDuringDecode $ Msg.decode msg

tryRecvAndDecode :: Aeson.FromJSON a => WS.Connection -> ExceptT Error IO a
tryRecvAndDecode conn = do
    msg <- lift $ (recv conn :: IO ByteString)
    tryRight $ decodeOrWrapError msg

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

    -- Move to repo/score state
    clientStateNextRepoOrEnd conn

clientStateNextRepoOrEnd :: WS.Connection -> ExceptT Error IO String
clientStateNextRepoOrEnd conn = do
    -- Receive a message
    msg <- lift $ (recv conn :: IO ByteString)

    -- Check whether it was a repo message, or a score message
    case Msg.decode msg :: Either Msg.MsgError Msg.MRepo of
        Right mRepo -> do
            -- repo -> loop over every instance
            let repoName = T.unpack $ Msg.mRepoName mRepo
            let repoGraph = git_repo_list_to_map $ Msg.mRepoDag mRepo
            let repoInstanceCount = Msg.mRepoInstanceCount mRepo
            lift $ putStrLn $ "solving repo: " ++ repoName
            clientStateRepo conn repoName repoGraph repoInstanceCount
        Left err ->
            case Msg.decode msg :: Either Msg.MsgError Msg.MScore of
                -- score -> loop over every instance
                Right mScore ->
                    ExceptT $ return $ Right $ show mScore
                -- neither -> some sort of server error
                Left err ->
                    ExceptT $ return $ Left $ ErrorServerError

clientStateRepo :: WS.Connection -> String -> GitGraph -> Int -> ExceptT Error IO String
clientStateRepo conn repoName g 0 = clientStateNextRepoOrEnd conn
clientStateRepo conn repoName g remainingInstances = do
    -- Receive an instance
    mInstance :: Msg.MInstance <- tryRecvAndDecode conn
    let cGood = Msg.mInstanceGood mInstance
    let cBad = Msg.mInstanceBad mInstance
    lift $ putStrLn $ repoName ++ ": received instance"

    -- Perform initial filter
    sg <- ExceptT $ return $ subgraph $ Algo.deleteSubgraph cGood g >>= Algo.subgraphRewriteParents cBad

    --lift $ putStrLn "filtered graph both sides"

    -- Q&A loop until we find the solution
    cSolution <- clientStateBisectLoop conn [cGood] cBad sg
    --cSolution <- ExceptT $ return $ Right $ cBad

    -- Send answer
    --lift $ print $ cSolution
    lift $ send conn $ Msg.MSolution cSolution

    -- And recurse for the remaining instances
    clientStateRepo conn repoName g (remainingInstances-1)

clientStateBisectLoop :: WS.Connection -> [GitCommit] -> GitCommit -> GitGraph -> ExceptT Error IO GitCommit
clientStateBisectLoop conn cGood cBad g =
    if Map.size g == 1 then ExceptT $ return $ Right $ cBad
    else do
        lift $ putStrLn $ "size: " ++ show (Map.size g)
        (cBisect, g') <- tryJust ErrorEncounteredAlgoErrorDuringBisectSelection $ AlgoOld.git_repo_select_bisect_with_limit 10000 cBad g
        lift $ send conn $ Msg.MQuestion cBisect
        mAnswer :: Msg.MAnswer <- tryRecvAndDecode conn
        case Msg.mAnswerCommitStatus mAnswer of
            Msg.CommitBad -> do
                g'' <- ExceptT $ return $ subgraph $ Algo.subgraph cBisect g'
                clientStateBisectLoop conn cGood cBisect g''
            Msg.CommitGood -> do
                g'' <- ExceptT $ return $ subgraph $ Algo.deleteSubgraph cBisect g' >>= Algo.subgraphRewriteParents cBad
                clientStateBisectLoop conn [cBisect] cBad g''

wrapAlgoError :: (Algo.Error -> Error) -> Either Algo.Error a -> Either Error a
wrapAlgoError e f = either (Left . e) Right f
subgraph :: Either Algo.Error a -> Either Error a
subgraph f = wrapAlgoError ErrorEncounteredAlgoErrorDuringSubgraph f
