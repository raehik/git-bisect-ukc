{-# LANGUAGE OverloadedStrings #-}

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
import Control.Error.Safe (tryRight)

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
    deriving (Show)

type ClientResult = Either NetError String
type Client = WS.Connection -> IO ClientResult

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

{-
client :: ClientConfig -> ClientConnection -> IO ClientResult
client cd conn = do
    let ca = clientConfigAuth cd
    putStrLn "authenticating..."
    send conn $ Msg.MAuth (clientAuthUser ca) (clientAuthToken ca)
    putStrLn "authenticated"
    msg <- recv conn
    case Msg.decode msg :: Either Msg.MsgError Msg.MRepo of
        Left err -> print err
        Right mRepo -> do
            msg <- recv conn
            case Msg.decode msg :: Either Msg.MsgError Msg.MInstance of
                Left err -> print err
                Right mInstance -> do
                    print mRepo
                    print mInstance
-}

--decodeOrWrapError msg = mapLeft NetErrorEncounteredMsgErrorDuringDecode $ Msg.decode msg
decodeOrWrapError msg = Left NetErrorUnspecified

-- all done in an ExceptT ClientResult IO a
-- lift wraps an IO a into our monad
-- return wraps an a into our monad
client :: ClientConfig -> WS.Connection -> IO ClientResult
client cc conn = runExceptT $ do
    lift $ putStrLn "client started"
    let cca = clientConfigAuth cc
    lift $ send conn $ Msg.MAuth (clientAuthUser cca) (clientAuthToken cca)
    msg <- lift $ (recv conn :: IO ByteString)
    mRepo <- tryRight $ (decodeOrWrapError msg :: Either NetError Msg.MRepo)
    --msg <- lift $ (recv conn :: IO ByteString)
    --mInstance <- return $ (decodeOrWrapError msg :: Either NetError Msg.MInstance)
    --lift $ print $ Msg.mInstanceGood mInstance
    --dag <- return $ maybe NetErrorEncounteredAlgoErrorDuringSubgraph id $ Algo.git_repo_sg_bad (Msg.mInstanceBad mInstance) (Msg.mRepoDag mRepo) >>= Algo.git_repo_sg_good (Msg.mInstanceGood mInstance) (Msg.mInstanceBad mInstance)
    lift $ print $ mRepo
    ExceptT $ return $ Left NetErrorUnimplemented

--    ExceptT $ return $ Right $ show mRepo
