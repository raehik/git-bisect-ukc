{-# LANGUAGE OverloadedStrings #-}

module GitBisect.Algo where

import GitBisect.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe

data Error
    = ErrorUnspecified
    | ErrorMissingReferencedCommit GitCommit
    deriving (Show)

maybeToEither e f = maybe (Left e) Right f

-- Common graph helper functions.
lookupInGraph c g = maybeToEither (ErrorMissingReferencedCommit c) $ Map.lookup c g
scheduleUnseen seen cs l = foldl scheduleUnseen' (seen, cs) l
    where
        scheduleUnseen' (seen, cs) c =
            if Set.notMember c seen
            then (Set.insert c seen, c:cs)
            else (seen, cs)

deleteSubgraph :: GitCommit -> GitGraph -> Either Error GitGraph
deleteSubgraph head g = deleteSubgraph' g (Set.singleton head) [head]
    where
        deleteSubgraph' g _ [] = Right g
        deleteSubgraph' g seen (c:cs) = do
            gge <- lookupInGraph c g
            let (g', seen', cs') = case gitGraphEntryAncestors gge of
                    Just ancs -> (foldl (flip Map.delete) g ancs, Set.union ancs seen, cs)
                    Nothing ->
                        let (seen', cs') = scheduleUnseen seen cs (gitGraphEntryParents gge)
                        in (Map.delete c g, seen', cs')
            deleteSubgraph' g' seen' cs'

subgraph :: GitCommit -> GitGraph -> Either Error GitGraph
subgraph head g = subgraph' Map.empty (Set.singleton head) [head]
    where
        subgraph' sg _ [] = Right sg
        subgraph' sg seen (c:cs) = do
            gge <- lookupInGraph c g
            let (seen', cs') = scheduleUnseen seen cs (gitGraphEntryParents gge)
            subgraph' (Map.insert c gge sg) seen' cs'
