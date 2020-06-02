{-# LANGUAGE OverloadedStrings #-}

module GitBisect.Algo where

import GitBisect.Types
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Sequence (Seq(..))

data Error
    = ErrorUnspecified
    | ErrorMissingReferencedCommit CommitID
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

subgraph :: CommitID -> CommitGraph -> Either Error CommitGraph
subgraph head g = subgraph' Map.empty (Set.singleton head) [head]
    where
        subgraph' sg _ [] = Right sg
        subgraph' sg seen (c:cs) = do
            gge <- lookupInGraph c g
            let (seen', cs') = scheduleUnseen seen cs (commitGraphEntryParents gge)
            subgraph' (Map.insert c gge sg) seen' cs'

-- Continue on missing commits.
subgraphForce :: CommitID -> CommitGraph -> CommitGraph
subgraphForce head g = traverse Map.empty (Set.singleton head) [head]
    where
        traverse sg _ [] = sg
        traverse sg seen (c:cs) =
            case Map.lookup c g of
                Nothing -> traverse sg seen cs
                Just gge ->
                    let (seen', cs') = scheduleUnseen seen cs (commitGraphEntryParents gge) in
                    traverse (Map.insert c gge sg) seen' cs'

deleteSubgraph :: CommitID -> CommitGraph -> Either Error CommitGraph
deleteSubgraph head g = deleteSubgraph' g (Set.singleton head) [head]
    where
        deleteSubgraph' g _ [] = Right g
        deleteSubgraph' g seen (c:cs) = do
            gge <- lookupInGraph c g
            let (g', seen', cs') = case commitGraphEntryAncestors gge of
                    Just ancs -> (foldl (flip Map.delete) g ancs, Set.union ancs seen, cs)
                    Nothing ->
                        let (seen', cs') = scheduleUnseen seen cs (commitGraphEntryParents gge)
                        in (Map.delete c g, seen', cs')
            deleteSubgraph' g' seen' cs'

-- Continue on missing commits. (They may have been removed earlier.)
deleteSubgraphForce :: CommitID -> CommitGraph -> CommitGraph
deleteSubgraphForce head g = deleteSubgraphForce' g (Set.singleton head) [head]
    where
        deleteSubgraphForce' g _ [] = g
        deleteSubgraphForce' g seen (c:cs) =
            case Map.lookup c g of
                Nothing -> deleteSubgraphForce' g seen cs
                Just gge ->
                    let (g', seen', cs') = case commitGraphEntryAncestors gge of
                            Just ancs -> (foldl (flip Map.delete) g ancs, Set.union ancs seen, cs)
                            Nothing ->
                                let (seen', cs') = scheduleUnseen seen cs (commitGraphEntryParents gge)
                                in (Map.delete c g, seen', cs')
                    in deleteSubgraphForce' g' seen' cs'

subgraphRewriteParents :: CommitID -> CommitGraph -> Either Error CommitGraph
subgraphRewriteParents head g = do
    gge <- lookupInGraph head g
    subgraphRewriteParents' (Map.insert head (presentParents g gge) g) (Set.singleton head) [head]
    where
        presentParents g gge =
            let cps' = filter (flip Map.member g) (commitGraphEntryParents gge)
            in CommitGraphEntry cps' Nothing
        subgraphRewriteParents' g _ [] = Right g
        subgraphRewriteParents' g seen (c:cs) = do
            gge <- lookupInGraph c g
            let gge' = presentParents g gge
            let (seen', cs') = scheduleUnseen seen cs (commitGraphEntryParents gge')
            subgraphRewriteParents' (Map.insert c gge' g) seen' cs'

subgraphForceInvalidateAncs :: CommitID -> CommitGraph -> CommitGraph
subgraphForceInvalidateAncs head g =
    traverse Map.empty (Set.singleton head) [head]
    where
        traverse sg _ [] = sg
        traverse sg seen (c:cs) =
            case Map.lookup c g of
                Nothing -> traverse sg seen cs
                Just gge ->
                    let cps = commitGraphEntryParents gge in
                    let (seen', cs') = scheduleUnseen seen cs cps in
                    traverse (Map.insert c (CommitGraphEntry cps Nothing) sg) seen' cs'

--selectBisectWithLimit rem_calcs head g =
--    selectBisectWithLimit' rem_calcs [head] g (Set.singleton head) (head, 0)

-- Case: Graph fully traversed. Return best commit so far (== overall best).
--selectBisectWithLimit' _ [] g _ (cBestCur, _) = Right (cBestCur, g)

-- Case: Calculations exhausted. Return best commit so far.
--selectBisectWithLimit' 0 _  g _ (cBestCur, _) = Right (cBestCur, g)

--selectBisectWithLimit' remCalcs (c:cs) g _ (cBestCur, _) = Right (cBestCur, g)

selectBisectIdeal :: CommitID -> CommitGraph -> (CommitID, CommitGraph)
selectBisectIdeal head g =
    selectBisectIdeal' [head] g (Set.empty) (head, 0)

selectBisectIdeal' []       g   _       (cBestCur, _)           = (cBestCur, g)
selectBisectIdeal' (c:cs)   g   done    (cBestCur, cBestRank)   =
    if Set.member c done then
        -- already calculated+checked, skip
        selectBisectIdeal' cs g done (cBestCur, cBestRank)
    else do
        case Map.lookup c g of
            Nothing -> selectBisectIdeal' cs g done (cBestCur, cBestRank)
            Just gge ->
                let cps = commitGraphEntryParents gge in
                case commitGraphEntryAncestors gge of
                    Nothing ->
                        -- ancestors not yet calculated
                        case foldl (calculateAncsOrSched g) (AncSet (Set.singleton c)) cps of
                            AncSched cSched ->
                                -- missing some parents ancestors, schedule them
                                -- then reschedule current commit
                                let cs' = revprepend (c:cs) cSched in
                                selectBisectIdeal' cs'      g   done (cBestCur, cBestRank)
                            AncSet cAncs ->
                                -- successfully calculated ancestors: reschedule
                                -- ourselves, we check in the other case
                                -- shouldn't be a big slowdown, keeps code neat
                                let g' = Map.insert c (CommitGraphEntry cps (Just cAncs)) g in
                                selectBisectIdeal' (c:cs)   g'  done (cBestCur, cBestRank)
                    Just cAncs ->
                        -- ancestors already calculated
                        let done' = Set.insert c done
                            cRank = min (Set.size cAncs) ((Map.size g) - (Set.size cAncs))
                            graphHalf = fromIntegral (Map.size g) / 2
                            (cBestCur', cBestRank') =
                                if cRank > cBestRank
                                then (c, cRank)
                                else (cBestCur, cBestRank)
                            cs' =
                                {-
                                -- don't schedule if an ideal commit can't be
                                -- found past here
                                if cRank > ceiling graphHalf
                                then revprepend cps cs
                                else cs
                                -} revprepend cps cs
                        in
                            if cRank > cBestRank then
                                -- better rank found
                                if cRank >= floor graphHalf then
                                    -- it's a ideal bisect: end early
                                    (c, g)
                                else selectBisectIdeal' cs' g done' (cBestCur', cBestRank')
                            else selectBisectIdeal' cs' g done' (cBestCur', cBestRank')

data EitherAncSetOrSched
    = AncSet (Set CommitID)
    | AncSched [CommitID]

calculateAncsOrSched g (AncSet curAncs) c =
    case Map.lookup c g of
        Nothing -> AncSet curAncs
        Just gge ->
            case commitGraphEntryAncestors gge of
                Nothing -> AncSched [c]
                Just cAncs -> AncSet (Set.union cAncs curAncs)

calculateAncsOrSched g (AncSched curSched) c =
    case Map.lookup c g of
        Nothing -> AncSched curSched
        Just gge ->
            case commitGraphEntryAncestors gge of
                Nothing -> AncSched (c:curSched)
                Just cAncs -> AncSched curSched

revprepend :: Foldable t => [a] -> t a -> [a]
revprepend = foldl (flip (:))

selectBisectBfsToHalfway :: CommitID -> CommitGraph -> Maybe CommitID
selectBisectBfsToHalfway head g =
    selectBisectBfsToHalfway' (Seq.singleton head) g (Set.empty) 0

selectBisectBfsToHalfway' Seq.Empty g seen nodeNum = Nothing
selectBisectBfsToHalfway' (c :<| queue) g seen nodeNum =
    case Map.lookup c g of
        Nothing -> selectBisectBfsToHalfway' queue g seen nodeNum
        Just gge ->
            if nodeNum == midpoint then
                Just c
            else
                let (seen', queue') = scheduleUnseenQueue seen queue (commitGraphEntryParents gge) in
                selectBisectBfsToHalfway' queue' g seen' (nodeNum+1)
    where
        midpoint = floor $ fromIntegral (Map.size g) / 2

scheduleUnseenQueue :: Set CommitID -> Seq CommitID -> [CommitID] -> (Set CommitID, Seq CommitID)
scheduleUnseenQueue seen queue l = foldl schedFold (seen, queue) l
    where
        schedFold (seen, queue) c =
            if Set.notMember c seen
            then (Set.insert c seen, queue Seq.|> c)
            else (seen, queue)
