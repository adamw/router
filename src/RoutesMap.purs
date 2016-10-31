module RoutesMap
  ( RouteIdMap
  , RoutesMap
  , create
  , empty
  ) where

import Prelude
import Route
import Data.Map as M
import Data.Set as S
import Data.Foldable (foldl)
import Data.Maybe (fromMaybe)
import Data.Pair (Pair)
import Data.Tuple (snd, fst, Tuple(Tuple))
import Data.Sequence (empty) as SQ

type RouteIdMap k = M.Map k (S.Set RouteId)

type CreateMap = Tuple (RouteIdMap StopId) (RouteIdMap (Pair StopId))

type RoutesMap =
  { selected :: S.Set StopId
  , perimeterRouteIds :: RouteIdMap StopId
  , roadRouteIds :: RouteIdMap (Pair StopId)
  }

create :: Routes -> S.Set StopId -> RoutesMap
create routes selectedStops = { selected: selectedStops
  , perimeterRouteIds: fst result
  , roadRouteIds: snd result
  } where
  addRouteId :: forall k. Ord k => RouteId -> RouteIdMap k -> k -> RouteIdMap k
  addRouteId c cm k = M.insert k (S.insert c $ fromMaybe S.empty $ M.lookup k cm) cm
  addRouteFragment :: RouteId -> CreateMap -> RouteFragment -> CreateMap
  addRouteFragment routeId (Tuple stopRouteIds roadStopIds) rf = let
    stops = [lastFragmentStop rf, firstFragmentStop rf]
    stopRouteIds' = foldl (addRouteId routeId) stopRouteIds stops
    roadStopIds' = foldl (addRouteId routeId) roadStopIds (fragmentRoads rf)
    in Tuple stopRouteIds' roadStopIds'
  addRoute :: CreateMap -> Route -> CreateMap
  addRoute cm r = foldl (addRouteFragment r.routeId) cm r.fragments
  addRoutes cm = foldl addRoute cm routes
  result = addRoutes $ Tuple M.empty M.empty

empty :: RoutesMap
empty = create SQ.empty S.empty
