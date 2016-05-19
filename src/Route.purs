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
  , roads
  ) where

import Prelude
import Data.Sequence as SQ
import Data.Sequence.NonEmpty as NE
import Data.Maybe
import Data.Pair
import Data.Tuple
import Data.Foldable(any)

newtype RouteId = RouteId Int

instance eqRouteId :: Eq RouteId where
  eq (RouteId id1) (RouteId id2) = eq id1 id2

instance ordRouteId :: Ord RouteId where
  compare (RouteId id1) (RouteId id2) = compare id1 id2

initialRouteId :: RouteId
initialRouteId = RouteId 1

nextRouteId :: RouteId -> RouteId
nextRouteId (RouteId n) = RouteId (n+1)

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
routeContains s { fragments = f } = any (fragmentContains s) f 

firstStop :: Route -> Maybe StopId
firstStop r = NE.head <$> SQ.head r.fragments

isFirstStop :: Route -> StopId -> Boolean
isFirstStop r s = firstStop r == Just s

lastStop :: Route -> Maybe StopId
lastStop r = NE.last <$> SQ.last r.fragments

addFragment :: RouteFragment -> Route -> Route
addFragment rf r = r { fragments = SQ.cons rf r.fragments }

removeLastFragment :: Route -> Route
removeLastFragment r@{ fragments = f } = r { fragments = fromMaybe f $ SQ.init f }

fragmentContains :: StopId -> RouteFragment -> Boolean
fragmentContains s rf = any (eq s) rf

firstFragmentStop :: RouteFragment -> StopId
firstFragmentStop rf = NE.head rf

lastFragmentStop :: RouteFragment -> StopId
lastFragmentStop rf = NE.last rf

roads :: RouteFragment -> SQ.Seq (Pair StopId)
roads rf = let
  ht = NE.uncons rf
  rds prev acc Nothing = acc
  rds prev acc (Just (Tuple curr tail)) = rds curr (SQ.snoc acc (Pair prev curr)) (SQ.uncons tail)
  in rds (fst ht) SQ.empty (SQ.uncons $ snd ht)
  
