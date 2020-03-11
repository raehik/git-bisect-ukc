{-# LANGUAGE OverloadedStrings #-}

module GitBisect.Network.Client where

import GitBisect.Types
import qualified GitBisect.Network.Messages as Msg
import GitBisect.Algo

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
    deriving (Show)

type ClientResult = Either NetError String
type Client = WS.Connection -> IO ClientResult

send :: Aeson.ToJSON a => WS.Connection -> a -> IO ()
send conn msg = WS.sendTextData conn $ Aeson.encode msg

--recv :: TODO a => ClientConnection -> IO a
recv = WS.receiveData

serverCfg_test = ServerConfig "129.12.44.229" 1234 "/"
clientCfg_bo207 = ClientConfig $ ClientAuth "bo207" "49ea39ac"

client_bo207 = client clientCfg_bo207

-- | Run a client against the given server.
run :: Client -> ServerConfig -> IO ClientResult
run c sc = WS.runClient (serverConfigHost sc) (serverConfigPort sc) (serverConfigPath sc) c

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

--decodeOrPrintError msg = Msg.decode msg -> IO ()

--client :: ClientConfig -> ClientConnection -> IO ClientResult
client cc conn = runExceptT $ do
    let cca = clientConfigAuth cc
    lift $ send conn $ Msg.MAuth (clientAuthUser cca) (clientAuthToken cca)
    msg <- lift $ (recv conn :: IO ByteString)
    ExceptT $ return $ Right "ok"
--    case Msg.decode msg :: Either Msg.MsgError Msg.MRepo of
--        Left err -> return $ Left $ NetErrorEncounteredMsgErrorDuringDecode err
--        Right mRepo -> return $ Right "ok"
--    --mRepo <- return $ either (Msg.decode msg :: Either Msg.MsgError Msg.MRepo) 
