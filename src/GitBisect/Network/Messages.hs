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

type MsgString = Text

data CommitStatus
    = CommitGood
    | CommitBad
    deriving (Show, Generic)
instance FromJSON CommitStatus where
    parseJSON (String "Good") = return CommitGood
    parseJSON (String "Bad") = return CommitBad
    parseJSON _ = fail "not a valid commit status"
instance ToJSON CommitStatus where
    toJSON CommitGood = String "Good"
    toJSON CommitBad = String "Bad"

data MsgError
    = MsgErrorAesonDecodeFailed String
    deriving (Show)

decode :: FromJSON a => ByteString -> Either MsgError a
decode msg = mapLeft MsgErrorAesonDecodeFailed (Data.Aeson.eitherDecode msg)

encode :: ToJSON a => a -> ByteString
encode = Data.Aeson.encode

data MAuth = MAuth {
    mAuthUser :: MsgString,
    mAuthToken :: MsgString
} deriving (Show, Generic)
instance ToJSON MAuth where
    toJSON MAuth{..} = object [
        "User" .= [mAuthUser, mAuthToken]
        ]

data MRepo = MRepo {
    mRepoName :: MsgString,
    mRepoInstanceCount :: Int,
    mRepoDag :: [(MsgString, [MsgString])]
} deriving (Show, Generic)
instance FromJSON MRepo where
    parseJSON = withObject "MRepo" $ \o -> do
        repo <- o .: "Repo"
        mRepoName <- repo .: "name"
        mRepoInstanceCount <- repo .: "instance_count"
        mRepoDag <- repo .: "dag"
        return MRepo{..}

data MInstance = MInstance {
    mInstanceGood :: MsgString,
    mInstanceBad :: MsgString
} deriving (Show, Generic)
instance FromJSON MInstance where
    parseJSON = withObject "MInstance" $ \o -> do
        inst <- o .: "Instance"
        mInstanceGood <- inst .: "good"
        mInstanceBad <- inst .: "bad"
        return MInstance{..}

{-
data JPDagEntry = JPDagEntry GitCommit [GitCommit] deriving (Show, Generic, ToJSON, FromJSON)
data JPProblem = JPProblem {
    jpProblemName :: MsgString,
    jpProblemGood :: GitCommit,
    jpProblemBad :: GitCommit,
    jpProblemDag :: [JSONPartDagEntry]
} deriving (Show, Generic, ToJSON, FromJSON)
data JMProblem = JMProblem {
    problem :: JPProblem
} deriving (Show, Generic)
instance ToJSON JMProblem where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = capitalize
    }
instance FromJSON JSONMsgProblem where
    parseJSON = withObject "JSONMsgProblem" $ \o -> do
        problem <- o .: "Problem"
        return JSONMsgProblem{..}

data JSONMsgQuestion = JSONMsgQuestion {
    question :: GitCommit
} deriving (Show, Generic)
instance ToJSON JSONMsgQuestion where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = capitalize
    }
instance FromJSON JSONMsgQuestion where
    parseJSON = withObject "JSONMsgQuestion" $ \o -> do
        question <- o .: "Question"
        return JSONMsgQuestion{..}

data JSONMsgAnswer = JSONMsgAnswer {
    answer :: GitCommitStatus
} deriving (Show, Generic)
instance ToJSON JSONMsgAnswer where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = capitalize
    }
instance FromJSON JSONMsgAnswer where
    parseJSON = withObject "JSONMsgAnswer" $ \o -> do
        answer <- o .: "Answer"
        return JSONMsgAnswer{..}

data JSONMsgSolution = JSONMsgSolution {
    solution :: GitCommit
} deriving (Show, Generic)
instance ToJSON JSONMsgSolution where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = capitalize
    }
instance FromJSON JSONMsgSolution where
    parseJSON = withObject "JSONMsgSolution" $ \o -> do
        solution <- o .: "Solution"
        return JSONMsgSolution{..}

-- Weird stuff going on here: Aeson comes a built-in instance for Map Text
-- a, and an instance for Maybe b. In particular, the Maybe instance gives you
-- Just a for a regular value, or Nothing for a null. Very handy.
data JSONPartScore = JSONPartScore (Map MsgString (Maybe Int)) deriving (Show, Generic, ToJSON, FromJSON)
data JSONMsgScore = JSONMsgScore {
    score :: JSONPartScore
} deriving (Show, Generic)
instance ToJSON JSONMsgScore where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = capitalize
    }
instance FromJSON JSONMsgScore where
    parseJSON = withObject "JSONMsgScore" $ \o -> do
        score <- o .: "Score"
        return JSONMsgScore{..}

data JPFileRepoRefAns = JPFileRepoRefAns {
    jpFileRepoRefAnsBug :: GitCommit,
    jpFileRepoRefAnsAllBad :: [GitCommit]
} deriving (Show, Generic)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 13} ''JPFileRepoRefAns)
data JSONMsgFileRepo = JSONMsgFileRepo JSONPartProblem JSONPartFileRepoRefAns deriving (Show, Generic, ToJSON, FromJSON)

--decode_file :: FilePath -> IO (Maybe JSONMsgFileRepo)
decode_file f = do
    json <- BL.readFile f
    return (decode json :: Maybe JSONMsgFileRepo)

-- Convert a repo in JSON representation to a GitGraph.
-- Makes no validity checks, and does not calculate ancestors.
git_json_repo_to_graph :: [JSONPartDagEntry] -> GitGraph
git_json_repo_to_graph = foldl (\g (JSONPartDagEntry c cps) -> Map.insert c (GitGraphEntry cps Nothing) g) Map.empty

capitalize (ch:chars) = (Char.toUpper ch):chars
decapitalize (ch:chars) = (Char.toLower ch):chars

-}
