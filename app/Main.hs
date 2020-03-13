module Main where

import GitBisect.Types
import qualified GitBisect.Network.Client as Client

main :: IO ()
main = do
    out <- Client.run Client.client_bo207 Client.serverCfg_submission
    case out of
        Left err -> putStrLn $ "ERROR: " ++ show err
        Right result -> putStrLn result
