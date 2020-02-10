{-# LANGUAGE OverloadedStrings #-}

module Network where

import qualified Network.WebSockets as WS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Control.Monad (forever, unless)
import Control.Monad.Trans (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as C

chan_stdin chan = do
    line <- getLine
    writeChan chan (WS.DataMessage (C.pack line))

--ws_send_chan :: WS.Connection -> Chan a -> IO ()
ws_send_chan conn chan = do
    msg <- readChan chan
    WS.sendTextData conn msg

--ws_app :: WS.ClientApp ()
ws_app conn = do
    chanR <- newChan :: IO (Chan WS.DataMessage)
    chanW <- newChan :: IO (Chan WS.DataMessage)

    putStrLn "Connected!"

    _ <- forkIO $ forever $ ws_send_chan conn chanR

    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
        liftIO $ T.putStrLn msg

    forever $ chan_stdin chanW
    WS.sendClose conn ("Bye!" :: Text)

ws_run = WS.runClient "echo.websocket.org" 80 "/" ws_app
