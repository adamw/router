module Route
  ( RouteId(RouteId)
  , initialRouteId
  , nextRouteId
  , StopId()
  , newStopId
  , RouteFragment
  , Route
  , emptyRoute
  , routeContains
  , firstStop  
  , isFirstStop  
  , lastStop
  , addFragment
  , removeLastFragment
  , firstFragmentStop
  , lastFragmentStop
  , fragmentRoads
  , isEmpty
  , isCircular
  ) where

import Prelude
import Data.Sequence as SQ
import Data.Sequence.NonEmpty as NE
import Data.Foldable (maximumBy, any)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Pair (Pair(..))
import Data.Tuple (Tuple(..), fst, snd)

newtype RouteId = RouteId Int

instance eqRouteId :: Eq RouteId where
  eq (RouteId id1) (RouteId id2) = eq id1 id2

instance ordRouteId :: Ord RouteId where
  compare (RouteId id1) (RouteId id2) = compare id1 id2

initialRouteId :: RouteId
initialRouteId = RouteId 1

nextRouteId :: SQ.Seq RouteId -> RouteId
nextRouteId seq = let
  ns = (\r -> case r of RouteId n -> n) <$> seq
  max = maximumBy compare ns
  in fromMaybe initialRouteId ((\n -> RouteId $ n+1) <$> max)

newtype StopId = StopId String

newStopId :: String -> StopId
newStopId = StopId

instance eqStopId :: Eq StopId where
  eq (StopId sid1) (StopId sid2) = eq sid1 sid2

instance ordStopId :: Ord StopId where
  compare (StopId sid1) (StopId sid2) = compare sid1 sid2

instance showStopId :: Show StopId where
  show (StopId sid) = sid

type RouteFragment = NE.Seq StopId -- consecutive stops

type Route =
  { routeId :: RouteId
  -- start of fragment n+1 should be == to end of fragment n
  , fragments :: SQ.Seq RouteFragment 
  }

emptyRoute :: RouteId -> Route
emptyRoute rid = { routeId: rid, fragments: SQ.empty }

routeContains :: StopId -> Route -> Boolean
routeContains s { fragments: f } = any (fragmentContains s) f 

firstStop :: Route -> Maybe StopId
firstStop r = NE.head <$> SQ.head r.fragments

isFirstStop :: Route -> StopId -> Boolean
isFirstStop r s = firstStop r == Just s

lastStop :: Route -> Maybe StopId
lastStop r = NE.last <$> SQ.last r.fragments

addFragment :: RouteFragment -> Route -> Route
addFragment rf r = r { fragments = SQ.snoc r.fragments rf }

removeLastFragment :: Route -> Route
removeLastFragment r@{ fragments: f } = r { fragments = fromMaybe f $ SQ.init f }

fragmentContains :: StopId -> RouteFragment -> Boolean
fragmentContains s rf = any (eq s) rf

firstFragmentStop :: RouteFragment -> StopId
firstFragmentStop rf = NE.head rf

lastFragmentStop :: RouteFragment -> StopId
lastFragmentStop rf = NE.last rf

fragmentRoads :: RouteFragment -> SQ.Seq (Pair StopId)
fragmentRoads rf = let
  ht = NE.uncons rf
  rds prev acc Nothing = acc
  rds prev acc (Just (Tuple curr tail)) = rds curr (SQ.snoc acc (Pair prev curr)) (SQ.uncons tail)
  in rds (fst ht) SQ.empty (SQ.uncons $ snd ht)
  
isEmpty :: Route -> Boolean
isEmpty { fragments } = SQ.null fragments

isCircular :: Route -> Boolean
isCircular r = (not $ isEmpty r) && firstStop r == lastStop r
