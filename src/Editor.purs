module Editor
  ( EditorState()
  , EditedRoute
  , Editor
  , RouteIdMap
  , RoutesMap
  , emptyEditor
  , selectStop
  , candidateStop
  , removeLastStop
  , createMap
  ) where

import Route
import City

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.Tuple
import Data.Foldable
import Data.Pair
import Data.Sequence as SQ
import Data.Coords

import Prelude

data EditorState
  = SelectInitial
  | FirstStopCandidate StopId
  | FirstStopSelected StopId
  | FragmentCandidate RouteFragment
  | SelectNext
    
type EditedRoute =
  { route :: Route
  , state :: EditorState
  }

type Editor =
  { city :: City
  , routes :: SQ.Seq Route
  , editedRoute :: EditedRoute
  }

type RouteIdMap k = M.Map k (S.Set RouteId)

type RoutesMap =
  { stopsCoords :: M.Map StopId Coords
  , selected :: S.Set StopId
  , perimeterRouteIds :: RouteIdMap StopId
  , roadRouteIds :: RouteIdMap (Pair StopId)
  -- editor state - so that changes can be detected & msgs displayed? or a user msg buffer?
  }

emptyEditor :: City -> Editor
emptyEditor c = { city: c, routes: SQ.empty, editedRoute: er } where
  er = { route: emptyRoute initialRouteId, state: SelectInitial }

setState       st e = e { editedRoute = e.editedRoute { state = st } }
setEditedRoute r  e = e { editedRoute = e.editedRoute { route = r  } }
                   
selectStop :: StopId -> Editor -> Editor
selectStop = let
  addFragmentBetween s1 s2 e = let 
    r' = addFragment (City.routeFragment s1 s2 e.city) e.editedRoute.route
    in setEditedRoute r' e
  whenRouteEmpty s = setState $ FirstStopSelected s
  whenChosenIsFirst last s = finishRoute <<< (addFragmentBetween last s)
  whenChosenIsNew = addFragmentBetween
  in chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew

candidateStop :: Maybe StopId -> Editor -> Editor
candidateStop Nothing e =
  case lastStop e.editedRoute.route of
    Nothing -> setState SelectInitial e
    Just _  -> setState SelectNext e
candidateStop (Just s) e = let
  candidateFragmentBetween s1 s2 e = let rf = City.routeFragment s1 s2 e.city
                                     in setState (FragmentCandidate rf) e
  whenRouteEmpty s = setState $ FirstStopCandidate s
  whenChosenIsFirst = candidateFragmentBetween
  whenChosenIsNew = candidateFragmentBetween
  in chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew s e

chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew s e@{ editedRoute = { route = r } } =
  case lastStop r of
    Nothing                    -> whenRouteEmpty s e
    Just last | isFirstStop r s && (last /= s) -> whenChosenIsFirst last s e
    Just _ | routeContains s r -> e
    Just last                  -> whenChosenIsNew last s e

finishRoute :: Editor -> Editor
finishRoute e@{ routes = rs } = e { routes = rs', editedRoute = er' } where
  rs' = SQ.cons e.editedRoute.route rs
  routeId' = nextRouteId e.editedRoute.route.routeId
  er' = { route: emptyRoute routeId', state: SelectInitial }

removeLastStop :: Editor -> Editor
removeLastStop e = setState s' <<< setEditedRoute r' $ e where
  r' = removeLastFragment e.editedRoute.route
  s' = if SQ.length r'.fragments == 0 then SelectInitial else SelectNext

type CreateMap = Tuple (RouteIdMap StopId) (RouteIdMap (Pair StopId))

createMap :: Editor -> RoutesMap
createMap e = { stopsCoords: stopsCoords e.city
  , selected: selectedStops e
  , perimeterRouteIds: fst result
  , roadRouteIds: snd result
  } where
  addRouteId :: forall k. Ord k => RouteId -> RouteIdMap k -> k -> RouteIdMap k
  addRouteId c cm k = M.insert k (S.insert c $ fromMaybe S.empty $ M.lookup k cm) cm
  addStop c (Tuple stopRouteIds roadStopIds) s = Tuple (addRouteId c stopRouteIds s) roadStopIds
  addRouteFragment :: RouteId -> CreateMap -> RouteFragment ->  CreateMap
  addRouteFragment routeId (Tuple stopRouteIds roadStopIds) rf = let
    stopRouteIds' = foldl (addRouteId routeId) stopRouteIds rf
    roadStopIds' = foldl (addRouteId routeId) roadStopIds (roads rf)
    in Tuple stopRouteIds' roadStopIds'
  addRoute :: CreateMap -> Route -> CreateMap
  addRoute cm r = foldl (addRouteFragment r.routeId) cm r.fragments
  addRoutes cm = foldl addRoute cm e.routes
  addEditedRoute cm@(Tuple stopRouteIds roadStopIds) = let
    c = e.editedRoute.route.routeId
    cm' = case e.editedRoute.state of
      FirstStopCandidate s -> addStop c cm s
      FirstStopSelected s  -> addStop c cm s
      FragmentCandidate rf -> addRouteFragment c cm rf
      _ -> cm
    in addRoute cm' e.editedRoute.route
  result = addRoutes <<< addEditedRoute $ Tuple M.empty M.empty
  
selectedStops :: Editor -> S.Set StopId
selectedStops e =
  case e.editedRoute.state of
    FirstStopCandidate s -> S.singleton s
    FirstStopSelected s  -> S.singleton s
    FragmentCandidate f  -> S.fromFoldable [ firstFragmentStop f, lastFragmentStop f ]
    SelectNext           -> S.fromFoldable $ lastStop e.editedRoute.route
    _                    -> S.empty
