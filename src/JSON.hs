{-# LANGUAGE OverloadedStrings #-}

-- allow Aeson to generate default FromJSON and ToJSON instances for us
{-# LANGUAGE DeriveGeneric #-}

-- enable even easier syntax for auto FromJSON, ToJSON instances
{-# LANGUAGE DeriveAnyClass #-}

-- nicer style for Aeson records (thx https://artyom.me/aeson)
{-# LANGUAGE RecordWildCards #-}

-- LOL
{-# LANGUAGE TemplateHaskell #-}

module JSON where

import Data

import GHC.Generics
import Data.Aeson
import Data.Aeson.TH
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Char as Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL

data GitCommitStatus
    = GitCommitGood
    | GitCommitBad
    deriving (Show, Generic)
instance FromJSON GitCommitStatus where
    parseJSON (String "Good") = return GitCommitGood
    parseJSON (String "Bad") = return GitCommitBad
    parseJSON _ = fail "not a valid commit status"
instance ToJSON GitCommitStatus where
    toJSON GitCommitGood = String "Good"
    toJSON GitCommitBad = String "Bad"

capitalize (ch:chars) = (Char.toUpper ch):chars
decapitalize (ch:chars) = (Char.toLower ch):chars

type JSONPartUser = [Text]
data JSONMsgUser = JSONMsgUser {
    user :: JSONPartUser
} deriving (Show, Generic)
instance ToJSON JSONMsgUser where
    toJSON = genericToJSON defaultOptions {
        fieldLabelModifier = capitalize
    }
instance FromJSON JSONMsgUser where
    parseJSON = withObject "JSONMsgUser" $ \o -> do
        user <- o .: "User"
        return JSONMsgUser{..}

data JSONPartDagEntry = JSONPartDagEntry GitCommit [GitCommit] deriving (Show, Generic, ToJSON, FromJSON)
data JSONPartProblem = JSONPartProblem {
    name :: Text,
    good :: GitCommit,
    bad :: GitCommit,
    dag :: [JSONPartDagEntry]
} deriving (Show, Generic, ToJSON, FromJSON)
data JSONMsgProblem = JSONMsgProblem {
    problem :: JSONPartProblem
} deriving (Show, Generic)
instance ToJSON JSONMsgProblem where
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
data JSONPartScore = JSONPartScore (Map Text (Maybe Int)) deriving (Show, Generic, ToJSON, FromJSON)
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

data JSONPartFileRepoRefAns = JSONPartFileRepoRefAns {
    fra_bug :: GitCommit,
    fra_all_bad :: [GitCommit]
} deriving (Show, Generic)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''JSONPartFileRepoRefAns)
data JSONMsgFileRepo = JSONMsgFileRepo JSONPartProblem JSONPartFileRepoRefAns deriving (Show, Generic, ToJSON, FromJSON)

--decode_file :: FilePath -> IO (Maybe JSONMsgFileRepo)
decode_file f = do
    json <- BL.readFile f
    return (decode json :: Maybe JSONMsgFileRepo)
