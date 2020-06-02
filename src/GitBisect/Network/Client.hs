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

serverCfg_test = ServerConfig "129.12.44.229" 1234 "/"
serverCfg_submission = ServerConfig "129.12.44.246" 1234 "/"

client_bo207 = client clientCfg_bo207
clientCfg_bo207 = ClientConfig $ ClientAuth "bo207" "49ea39ac"

send :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
send conn msg = do
    WS.sendTextData conn $ Aeson.encode msg

recv :: WS.WebSocketsData a => WS.Connection -> IO a
recv conn = do
    d <- WS.receiveData conn
    return d

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
    lift $ putStrLn $ "receiving next repo/score..."
    msg <- lift $ (recv conn :: IO ByteString)
    lift $ putStrLn $ "decoding repo/score..."

    -- Check whether it was a repo message, or a score message
    case Msg.decode msg :: Either Msg.Error Msg.MRepo of
        Right mRepo -> do
            -- repo -> loop over every instance
            let repoName = T.unpack $ Msg.mRepoName mRepo
            let repoInstanceCount = Msg.mRepoInstanceCount mRepo
            lift $ putStrLn $ "building internal graph..."
            let (graph, convCommitNetToInt) = Msg.dagToMap $ Msg.mRepoDag mRepo
            lift $ putStrLn $ "begin repo: " ++ repoName
            clientStateRepo conn repoName graph convCommitNetToInt repoInstanceCount
        Left err ->
            case Msg.decode msg :: Either Msg.Error Msg.MScore of
                -- score -> loop over every instance
                Right mScore ->
                    tryRight $ Right $ show mScore
                -- neither -> some sort of server error
                Left err ->
                    tryRight $ Left $ ErrorServerError

foldToIntGraph textCommitToInt textGraph textK textV intGraph =
    let intK = textCommitToInt textK in
    let intVParents = map textCommitToInt (commitGraphEntryParents textV) in
    Map.insert intK (CommitGraphEntry intVParents Nothing) intGraph

clientStateRepo :: WS.Connection -> String -> CommitGraph -> (Map Text CommitID) -> Int -> ExceptT Error IO String
clientStateRepo conn repoName g conv 0 = clientStateNextRepoOrEnd conn
clientStateRepo conn repoName g convToIntMap remainingInstances = do
    --lift $ print g
    let convToInt = (Map.!) convToIntMap
    -- Receive an instance
    mInstance :: Msg.MInstance <- tryRecvAndDecode conn
    let cGood = convToInt $ Msg.mInstanceGood mInstance
    let cBad = convToInt $ Msg.mInstanceBad mInstance
    lift $ putStrLn $ repoName ++ ": remaining instances: " ++ show remainingInstances

    -- Perform initial filter
    lift $ putStrLn $ "initial filter..."
    -- NOTE: *can* skip the subgraphing **IFF** you don't need the graph size on
    -- the first run
    --let sg = Algo.deleteSubgraphForce cGood g
    let sg = Algo.subgraphForce cBad (Algo.deleteSubgraphForce cGood g)

    -- Solve instance and send answer
    lift $ putStrLn $ "starting query loop..."
    let convToNet = (Map.!) $ invertBijection convToIntMap
    cSolution <- clientStateInstance conn [cGood] cBad sg convToNet 30

    -- And recurse for the remaining instances
    clientStateRepo conn repoName g convToIntMap (remainingInstances-1)

-- Cheers to subttle https://stackoverflow.com/questions/21538903/how-can-i-elegantly-invert-a-maps-keys-and-values
invertBijection :: (Ord k, Ord v) => Map k v -> Map v k
invertBijection = Map.foldrWithKey (flip Map.insert) Map.empty

-- Note that guard order matters greatly here - we need to check the map size
-- before the remaining questions.
clientStateInstance :: WS.Connection -> [CommitID] -> CommitID -> CommitGraph -> (CommitID -> Text) -> Int -> ExceptT Error IO (Maybe CommitID)
clientStateInstance conn cGood cBad g conv remQs
    | Map.size g == 1 = do
        -- Send answer
        lift $ putStrLn $ "solution: " ++ T.unpack (conv cBad)
        lift $ send conn $ Msg.MSolution (conv cBad)
        tryRight $ Right $ Just cBad
    | remQs == 0 = do
        -- Ran out of questions
        lift $ putStrLn $ "ran out of questions, giving up"
        lift $ send conn $ Msg.MGiveUp
        tryRight $ Right $ Nothing
    | Map.size g <= 10000 =
        -- Repo considered small enough to run slow ideal bisect algorithm.
        let (cBisect, g') = Algo.selectBisectIdeal cBad g in
        askAndFilterWithAncestorInvalidation cBisect g'
    | otherwise =
        -- Run an "eh" algorithm that often finds a good result (dependent on
        -- repo shape).
        case Algo.selectBisectBfsToHalfway cBad g of
            Nothing -> tryRight $ Left $ ErrorUnspecified
            Just cBisect -> do
                askAndFilter cBisect g
    where
        askAndFilter cBisect g = do
            lift $ send conn $ Msg.MQuestion (conv cBisect)
            mAnswer :: Msg.MAnswer <- tryRecvAndDecode conn
            case Msg.mAnswerCommitStatus mAnswer of
                CommitBad -> do
                    let g' = Algo.subgraphForce cBisect g
                    lift $ putStrLn $ T.unpack (conv cBisect) ++ ": bad  (" ++ show (Map.size g') ++ ")"
                    clientStateInstance conn cGood cBisect g' conv (remQs-1)
                CommitGood -> do
                    let g' = Algo.deleteSubgraphForce cBisect g
                    lift $ putStrLn $ T.unpack (conv cBisect) ++ ": good (" ++ show (Map.size g') ++ ")"
                    clientStateInstance conn [cBisect] cBad g' conv (remQs-1)
        askAndFilterWithAncestorInvalidation cBisect g = do
            lift $ send conn $ Msg.MQuestion (conv cBisect)
            mAnswer :: Msg.MAnswer <- tryRecvAndDecode conn
            case Msg.mAnswerCommitStatus mAnswer of
                CommitBad -> do
                    let g' = Algo.subgraphForce cBisect g
                    lift $ putStrLn $ T.unpack (conv cBisect) ++ ": bad  (" ++ show (Map.size g') ++ ")"
                    clientStateInstance conn cGood cBisect g' conv (remQs-1)
                CommitGood -> do
                    let g' = Algo.deleteSubgraphForce cBisect g
                    let g'' = Algo.subgraphForceInvalidateAncs cBad g'
                    lift $ putStrLn $ T.unpack (conv cBisect) ++ ": good (" ++ show (Map.size g'') ++ ")"
                    clientStateInstance conn [cBisect] cBad g'' conv (remQs-1)


subgraph f = mapLeft ErrorEncounteredAlgoErrorDuringSubgraph f

bisectRandom g = do
    rnd <- Random.randomRIO (0, (Map.size g)-1)
    let randomCommit = Map.keys g !! rnd
    return randomCommit
