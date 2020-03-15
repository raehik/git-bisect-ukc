{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module GitBisect.Parser where

-- Note that the YAML library uses strict ByteStrings, where as the JSON library
-- Aeson uses lazy. Looks like the YAML library is a wrapper around a C lib?

import GitBisect.Types
import qualified GitBisect.Algo as Algo
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Yaml
import qualified Data.ByteString as BS
import Data.Either.Combinators (mapLeft)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import qualified System.Random as Random
import Control.Exception

data GitRepo = GitRepo {
    gitRepoName :: Text,
    gitRepoGraph :: Map CommitID [CommitID],
    gitRepoInstances :: [GitRepoInstance]
} deriving (Show)
instance FromJSON GitRepo where
    parseJSON = withObject "GitRepo" $ \o -> do
        gitRepoName <- o .: "name"
        gitRepoGraph <- o .: "graph"
        gitRepoInstances <- o .: "instances"
        return GitRepo{..}

data GitRepoInstance = GitRepoInstance {
    gitRepoInstanceName :: Text,
    gitRepoInstanceGood :: CommitID,
    gitRepoInstanceBad :: CommitID,
    gitRepoInstanceBads :: Set CommitID
} deriving (Show)
instance FromJSON GitRepoInstance where
    parseJSON = withObject "GitRepoInstance" $ \o -> do
        gitRepoInstanceName <- o .: "name"
        gitRepoInstanceGood <- o .: "good"
        gitRepoInstanceBad <- o .: "bad"
        badsList <- o .: "bads"
        let gitRepoInstanceBads = Set.fromList badsList
        return GitRepoInstance{..}

data Error
    = ErrorYamlDecodeFailed ParseException
    | ErrorEncounteredAlgoErrorDuringSubgraph Algo.Error
    | ErrorWhenReadingFile IOException
    deriving (Show)

type RepoSolutions = (Text, [InstanceSolution])
type InstanceSolution = (Text, Either Error Solution)
type Solution = (CommitID, Int)

-- Read a file or safely error out with a message on failure.
readFileEither :: FilePath -> ExceptT Error IO BS.ByteString
readFileEither file =
    ExceptT $ catch (Right <$> BS.readFile file) (\(e :: IOException) -> Left <$> (return $ ErrorWhenReadingFile e))

processRepoFile :: FilePath -> IO (Either Error [RepoSolutions])
processRepoFile file = runExceptT $ do
    repos <- parseRepoFile file
    solutions <- lift $ mapM (\v -> solveRepo v >>= (\x -> return (gitRepoName v, x))) repos
    ExceptT $ return $ Right solutions

parseRepoFile :: FilePath -> ExceptT Error IO [GitRepo]
parseRepoFile file = do
    bytes <- readFileEither file
    ExceptT $ return $ mapLeft ErrorYamlDecodeFailed $ decodeEither' bytes

convertGraph graph = Map.map (\cps -> CommitGraphEntry cps Nothing) graph

solveRepo :: GitRepo -> IO [InstanceSolution]
solveRepo repo = do
    let graph = convertGraph $ gitRepoGraph repo
    mapM (f graph) (gitRepoInstances repo)
    where
        f graph inst = do
            sol <- runExceptT $ solveInstance graph inst
            return $ prependName (gitRepoInstanceName inst) sol
        prependName :: Text -> Either Error Solution -> InstanceSolution
        prependName name =
            either (\err -> (name, Left err)) (\sol -> (name, Right sol))

solveInstance :: CommitGraph -> GitRepoInstance -> ExceptT Error IO Solution
solveInstance graph inst = do
    lift $ putStrLn "NEXT"
    solveInstance' graph inst [(gitRepoInstanceGood inst)] (gitRepoInstanceBad inst) 0

solveInstance' graph inst cGood cBad score
    | commits == 1 = ExceptT $ return $ Right (cBad, score)
    | otherwise = do
        rnd <- lift $ Random.randomRIO (0, commits-1)
        let cBisect = Map.keys graph !! rnd
        if Set.member cBisect (gitRepoInstanceBads inst) then do
            lift $ putStrLn $ T.unpack cBisect ++ ": bad"
            graph' <- ExceptT $ return $ subgraph $ Algo.subgraph cBisect graph
            solveInstance' graph' inst cGood cBisect (score+1)
        else do
            lift $ putStrLn $ T.unpack cBisect ++ ": good"
            let graph' = Algo.deleteSubgraphForce cBisect graph
            graph'' <- ExceptT $ return $ subgraph $ Algo.subgraphRewriteParents cBad graph'
            solveInstance' graph'' inst [cBisect] cBad (score+1)
    where
        commits = Map.size graph

wrapAlgoError e f = either (Left . e) Right f
subgraph f = wrapAlgoError ErrorEncounteredAlgoErrorDuringSubgraph f
