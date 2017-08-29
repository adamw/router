module Route
  ( RouteId(RouteId)
  , initialRouteId
  , nextRouteId
  , StopId()
  , newStopId
  , RouteFragment
  , Route
  , Routes
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
import Data.Foldable (class Foldable, maximumBy, any, foldl)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Pair (Pair(..))
import Data.Tuple (Tuple(..), fst)
import Data.NonEmpty as NE
import Data.Array as A
import Data.List as L

newtype RouteId = RouteId Int

instance eqRouteId :: Eq RouteId where
  eq (RouteId id1) (RouteId id2) = eq id1 id2

instance ordRouteId :: Ord RouteId where
  compare (RouteId id1) (RouteId id2) = compare id1 id2

initialRouteId :: RouteId
initialRouteId = RouteId 1

nextRouteId :: forall f. Functor f => Foldable f => f RouteId -> RouteId
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

type RouteFragment = NE.NonEmpty Array StopId -- consecutive stops

type Route =
  { routeId :: RouteId
  -- start of fragment n+1 should be == to end of fragment n
  , fragments :: Array RouteFragment 
  }

type Routes = Array Route

emptyRoute :: RouteId -> Route
emptyRoute rid = { routeId: rid, fragments: [] }

routeContains :: StopId -> Route -> Boolean
routeContains s { fragments: f } = any (fragmentContains s) f 

firstStop :: Route -> Maybe StopId
firstStop r = NE.head <$> A.head r.fragments

isFirstStop :: Route -> StopId -> Boolean
isFirstStop r s = firstStop r == Just s

lastStop :: Route -> Maybe StopId
lastStop r = lastFragmentStop <$> A.last r.fragments

addFragment :: RouteFragment -> Route -> Route
addFragment rf r = r { fragments = A.snoc r.fragments rf }

removeLastFragment :: Route -> Route
removeLastFragment r@{ fragments: f } = r { fragments = fromMaybe f $ A.init f }

fragmentContains :: StopId -> RouteFragment -> Boolean
fragmentContains s rf = any (eq s) rf

firstFragmentStop :: RouteFragment -> StopId
firstFragmentStop rf = NE.head rf

lastFragmentStop :: RouteFragment -> StopId
lastFragmentStop rf = fromMaybe (NE.head rf) $ A.last (NE.tail rf)

fragmentRoads :: RouteFragment -> Array (Pair StopId)
fragmentRoads rf = let
  
  add (Tuple acc prev) curr = Tuple (L.Cons (Pair prev curr) acc) curr
  in A.fromFoldable $ fst $ foldl add (Tuple L.Nil (NE.head rf)) (NE.tail rf)
  
isEmpty :: Route -> Boolean
isEmpty { fragments } = A.null fragments

isCircular :: Route -> Boolean
isCircular r = (not $ isEmpty r) && firstStop r == lastStop r
