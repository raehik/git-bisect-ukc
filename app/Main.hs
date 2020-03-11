module Main where

import GitBisect.Types
import qualified GitBisect.Network.Client as Client

main :: IO ()
main = Client.run Client.client_bo207 Client.serverCfg_test >>= print
