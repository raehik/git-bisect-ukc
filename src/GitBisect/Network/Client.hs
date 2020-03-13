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
import qualified System.Random as Random

data ClientConfig = ClientConfig {
    clientConfigAuth :: ClientAuth
} deriving (Show)

data ClientAuth = ClientAuth {
    clientAuthUser :: Text,
    clientAuthToken :: Text
} deriving (Show)

data ServerConfig = ServerConfig {
    serverConfigHost :: Text,
    serverConfigPort :: Int,
    serverConfigPath :: Text
} deriving (Show)

data Error
    = ErrorUnspecified
    | ErrorEncounteredMsgErrorDuringDecode Msg.Error
    | ErrorUnimplemented
    | ErrorEncounteredAlgoErrorDuringSubgraph Algo.Error
    | ErrorEncounteredAlgoErrorDuringBisectSelection
    | ErrorServerError
    deriving (Show)

type ClientResult = Either Error String
type Client = WS.Connection -> IO ClientResult

-- Initialise repo map with empty ancestors.
git_repo_list_to_map :: [(GitCommit, [GitCommit])] -> GitGraph
git_repo_list_to_map l = foldl (\m (c, c_parents) -> Map.insert c (GitGraphEntry c_parents Nothing) m) Map.empty l

send :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
send conn msg = do
    WS.sendTextData conn $ Aeson.encode msg

recv :: WS.WebSocketsData a => WS.Connection -> IO a
recv conn = do
    d <- WS.receiveData conn
    return d

serverCfg_test = ServerConfig "129.12.44.229" 1234 "/"
clientCfg_bo207 = ClientConfig $ ClientAuth "bo207" "49ea39ac"

client_bo207 = client clientCfg_bo207

-- | Run a client against the given server.
run :: Client -> ServerConfig -> IO ClientResult
run c sc = do
    putStrLn "starting client..."
    WS.runClient (T.unpack $ serverConfigHost sc) (serverConfigPort sc) (T.unpack $ serverConfigPath sc) c

tryRecvAndDecode :: Aeson.FromJSON a => WS.Connection -> ExceptT Error IO a
tryRecvAndDecode conn = do
    msg <- lift $ (recv conn :: IO ByteString)
    tryRight $ mapLeft ErrorEncounteredMsgErrorDuringDecode $ Msg.decode msg

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
    lift $ putStrLn $ "receiving next repo..."
    msg <- lift $ (recv conn :: IO ByteString)

    -- Check whether it was a repo message, or a score message
    case Msg.decode msg :: Either Msg.Error Msg.MRepo of
        Right mRepo -> do
            -- repo -> loop over every instance
            let repoName = T.unpack $ Msg.mRepoName mRepo
            let repoGraph = git_repo_list_to_map $ Msg.mRepoDag mRepo
            let repoInstanceCount = Msg.mRepoInstanceCount mRepo
            lift $ putStrLn $ "solving repo: " ++ repoName
            clientStateRepo conn repoName repoGraph repoInstanceCount
        Left err ->
            case Msg.decode msg :: Either Msg.Error Msg.MScore of
                -- score -> loop over every instance
                Right mScore ->
                    tryRight $ Right $ show mScore
                -- neither -> some sort of server error
                Left err ->
                    tryRight $ Left $ ErrorServerError

clientStateRepo :: WS.Connection -> String -> GitGraph -> Int -> ExceptT Error IO String
clientStateRepo conn repoName g 0 = clientStateNextRepoOrEnd conn
clientStateRepo conn repoName g remainingInstances = do
    -- Receive an instance
    mInstance :: Msg.MInstance <- tryRecvAndDecode conn
    let cGood = Msg.mInstanceGood mInstance
    let cBad = Msg.mInstanceBad mInstance
    lift $ putStrLn $ repoName ++ ": starting instance..."

    -- Perform initial filter
    sg <- tryRight $ subgraph $ Algo.deleteSubgraph cGood g >>= Algo.subgraphRewriteParents cBad

    -- Solve instance and send answer
    cSolution <- clientStateInstance conn [cGood] cBad sg 30

    -- And recurse for the remaining instances
    clientStateRepo conn repoName g (remainingInstances-1)

-- Note that guard order matters greatly here - we need to check the map size
-- before the remaining questions.
clientStateInstance :: WS.Connection -> [GitCommit] -> GitCommit -> GitGraph -> Int -> ExceptT Error IO (Maybe GitCommit)
clientStateInstance conn cGood cBad g remQs
    | Map.size g == 1 = do
        -- Send answer
        lift $ putStrLn $ "solution: " ++ T.unpack cBad
        lift $ send conn $ Msg.MSolution cBad
        tryRight $ Right $ Just cBad
    | remQs == 0 = do
        -- Ran out of questions
        lift $ putStrLn $ "ran out of questions, giving up"
        lift $ send conn $ Msg.MGiveUp
        tryRight $ Right $ Nothing
    | otherwise = do
        -- Select bisect commit
        --(cBisect, g') <- tryJust ErrorEncounteredAlgoErrorDuringBisectSelection $ AlgoOld.git_repo_select_bisect_with_limit 10000 cBad g
        (cBisect, g') <- lift $ bisectRandom g

        -- Query its status and recurse with an accordingly filtered graph
        lift $ send conn $ Msg.MQuestion cBisect
        mAnswer :: Msg.MAnswer <- tryRecvAndDecode conn
        case Msg.mAnswerCommitStatus mAnswer of
            Msg.CommitBad -> do
                --g'' <- ExceptT $ return $ subgraph $ Algo.subgraph cBisect g'
                g'' <- tryRight $ subgraph $ Algo.subgraphRewriteParents cBisect g' >>= Algo.subgraph cBisect
                lift $ putStrLn $ T.unpack cBisect ++ ": bad  (" ++ show (Map.size g'') ++ ")"
                clientStateInstance conn cGood cBisect g'' (remQs-1)
            Msg.CommitGood -> do
                --g'' <- ExceptT $ return $ subgraph $ Algo.deleteSubgraph cBisect g' >>= Algo.subgraphRewriteParents cBad
                let g'' = Algo.deleteSubgraphForce cBisect g'
                g''' <- tryRight $ subgraph $ Algo.subgraphRewriteParents cBad g''
                lift $ putStrLn $ T.unpack cBisect ++ ": good (" ++ show (Map.size g''') ++ ")"
                clientStateInstance conn [cBisect] cBad g''' (remQs-1)

wrapAlgoError :: (Algo.Error -> Error) -> Either Algo.Error a -> Either Error a
wrapAlgoError e f = either (Left . e) Right f
subgraph :: Either Algo.Error a -> Either Error a
subgraph f = wrapAlgoError ErrorEncounteredAlgoErrorDuringSubgraph f

bisectRandom g = do
    rnd <- Random.randomRIO (0, (Map.size g)-1)
    let randomCommit = Map.keys g !! rnd
    return (randomCommit, g)
