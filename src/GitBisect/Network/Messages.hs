{-# LANGUAGE OverloadedStrings #-}

-- allow Aeson to generate default FromJSON and ToJSON instances for us
{-# LANGUAGE DeriveGeneric #-}

-- enable even easier syntax for auto FromJSON, ToJSON instances
{-# LANGUAGE DeriveAnyClass #-}

-- nicer style for Aeson records (thx https://artyom.me/aeson)
{-# LANGUAGE RecordWildCards #-}

-- LOL
{-# LANGUAGE TemplateHaskell #-}

module GitBisect.Network.Messages where

import GitBisect.Types
import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Char as Char
import Data.Text (Text)
import Data.ByteString.Lazy (ByteString)
import Data.Either.Combinators (mapLeft)
import Data.Scientific as Scientific

instance FromJSON CommitStatus where
    parseJSON (String "Good") = return CommitGood
    parseJSON (String "Bad") = return CommitBad
    parseJSON _ = fail "not a valid commit status"
instance ToJSON CommitStatus where
    toJSON CommitGood = String "Good"
    toJSON CommitBad = String "Bad"

data Error
    = ErrorAesonDecodeFailed String
    deriving (Show)

type NetCommitID = Text
type NetCommitGraphNode = (NetCommitID, [NetCommitID])
decode :: FromJSON a => ByteString -> Either Error a
decode msg = mapLeft ErrorAesonDecodeFailed (Data.Aeson.eitherDecode msg)

encode :: ToJSON a => a -> ByteString
encode = Data.Aeson.encode

data MAuth = MAuth {
    mAuthUser :: Text,
    mAuthToken :: Text
} deriving (Show, Generic)
instance ToJSON MAuth where
    toJSON MAuth{..} = object [
        "User" .= [mAuthUser, mAuthToken]
        ]

data MRepo = MRepo {
    mRepoName :: Text,
    mRepoInstanceCount :: Int,
    mRepoDag :: [NetCommitGraphNode]
} deriving (Show, Generic)
instance FromJSON MRepo where
    parseJSON = withObject "MRepo" $ \o -> do
        repo <- o .: "Repo"
        mRepoName <- repo .: "name"
        mRepoInstanceCount <- repo .: "instance_count"
        mRepoDag <- repo .: "dag"
        return MRepo{..}

data MInstance = MInstance {
    mInstanceGood :: Text,
    mInstanceBad :: Text
} deriving (Show, Generic)
instance FromJSON MInstance where
    parseJSON = withObject "MInstance" $ \o -> do
        inst <- o .: "Instance"
        mInstanceGood <- inst .: "good"
        mInstanceBad <- inst .: "bad"
        return MInstance{..}

data MQuestion = MQuestion {
    mQuestionCommit :: Text
} deriving (Show, Generic)
instance ToJSON MQuestion where
    toJSON MQuestion{..} = object [
        "Question" .= mQuestionCommit
        ]

data MAnswer = MAnswer {
    mAnswerCommitStatus :: CommitStatus
} deriving (Show, Generic)
instance FromJSON MAnswer where
    parseJSON = withObject "MAnswer" $ \o -> do
        mAnswerCommitStatus <- o .: "Answer"
        return MAnswer{..}

data MSolution = MSolution {
    mSolutionCommit :: NetCommitID
} deriving (Show, Generic)
instance ToJSON MSolution where
    toJSON MSolution{..} = object [
        "Solution" .= mSolutionCommit
        ]

data MScore = MScore {
    mScoreScores :: Map NetCommitID ProblemScore
} deriving (Show, Generic)
instance FromJSON MScore where
    parseJSON = withObject "MScore" $ \o -> do
        mScoreScores <- o .: "Score"
        return MScore{..}

data ProblemScore
    = ProblemScoreWrong
    | ProblemScoreGaveUp
    | ProblemScoreCorrect Int
    deriving (Show, Generic)
instance FromJSON ProblemScore where
    parseJSON (String "Wrong") = return ProblemScoreWrong
    parseJSON (String "GaveUp") = return ProblemScoreGaveUp
    parseJSON (Object obj) = do
        (Number score) <- obj .: "Correct"
        return $ ProblemScoreCorrect $ fromIntegral $ Scientific.coefficient score
    parseJSON _ = fail "not a valid problem score"

data MGiveUp = MGiveUp deriving (Show, Generic)
instance ToJSON MGiveUp where
    toJSON MGiveUp = String "GiveUp"

-- Initialise graph with empty ancestors.
--dagToMap :: [(NetCommitID, [NetCommitID])] -> CommitGraph
--dagToMap = foldl (\m (c, cps) -> Map.insert c (CommitGraphEntry cps Nothing) m) Map.empty

-- Convert network graph to internal, and provide a conversion map.
dagToMap :: [NetCommitGraphNode] -> (CommitGraph, Map NetCommitID CommitID)
dagToMap netGraph = (\(a,b,c) -> (a,b)) $ foldl graphFold (Map.empty, Map.empty, 0) netGraph
    where
        graphFold (m, mConv, acc) (tC, tCps) =
            let (iCps, mConv', acc') = foldl nodeFold ([], mConv, acc) tCps in
            let gge = (CommitGraphEntry iCps Nothing) in
            case Map.lookup tC mConv' of
                Nothing ->
                    let mConv'' = Map.insert tC acc' mConv' in
                    (Map.insert acc' gge m, mConv'', acc'+1)
                Just iC ->
                    (Map.insert iC gge m, mConv', acc')
        nodeFold (iCps, mConv, acc) tC =
            case Map.lookup tC mConv of
                Just iC -> ((iC:iCps), mConv, acc)
                Nothing -> ((acc:iCps), Map.insert tC acc mConv, acc+1)
