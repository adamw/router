module Editor
  ( EditorState(..)
  , EditedRoute
  , Editor
  , RouteIdMap
  , RoutesMap
  , emptyEditor
  , selectStop
  , candidateStop
  , removeLastStop
  , finishRoute
  , createMap
  ) where

import Route
import City
import Data.Maybe
import Data.Tuple
import Data.Foldable
import Data.Pair
import Prelude
import Data.Map as M
import Data.Sequence as SQ
import Data.Set as S

data EditorState
  = SelectInitial
  | FirstStopCandidate StopId
  | FirstStopSelected StopId
  | FragmentCandidate EditorState StopId StopId RouteFragment
  | SelectNext StopId
    
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
  { selected :: S.Set StopId
  , perimeterRouteIds :: RouteIdMap StopId
  , roadRouteIds :: RouteIdMap (Pair StopId)
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
    in setState (SelectNext s2) $ setEditedRoute r' e
  whenRouteEmpty s = setState (FirstStopSelected s)
  whenChosenIsFirst s1 s2 = finishRoute <<< (addFragmentBetween s1 s2)
  whenChosenIsNew = addFragmentBetween
  in chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew

candidateStop :: Maybe StopId -> Editor -> Editor
candidateStop Nothing e =
  case e.editedRoute.state of
    FirstStopCandidate _         -> setState SelectInitial e
    FragmentCandidate prev _ _ _ -> setState prev e
    _                            -> e
candidateStop (Just s) e = let
  candidateFragmentBetween s1 s2 e = let
    rf = City.routeFragment s1 s2 e.city
    newState = FragmentCandidate e.editedRoute.state s1 s2 rf
    in setState newState e
  whenRouteEmpty s = setState (FirstStopCandidate s)
  whenChosenIsFirst = candidateFragmentBetween
  whenChosenIsNew = candidateFragmentBetween
  in chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew s e

chooseStop whenRouteEmpty whenChosenIsFirst whenChosenIsNew s e@{ editedRoute = { route = r } } =
  let
    chooseFragment from = if (isFirstStop r s) && (from /= s)
      then whenChosenIsFirst from s e
      else if routeContains s r then e else whenChosenIsNew from s e
  in case e.editedRoute.state of
    SelectInitial -> whenRouteEmpty s e
    FirstStopCandidate _ -> whenRouteEmpty s e
    FirstStopSelected from -> chooseFragment from
    FragmentCandidate _ from _ _ -> chooseFragment from
    SelectNext last -> chooseFragment last

finishRoute :: Editor -> Editor
finishRoute e@{ routes = rs } = e { routes = rs', editedRoute = er' } where
  rs' = SQ.snoc rs e.editedRoute.route
  routeId' = nextRouteId e.editedRoute.route.routeId
  er' = { route: emptyRoute routeId', state: SelectInitial }

removeLastStop :: Editor -> Editor
removeLastStop e = setState s' <<< setEditedRoute r' $ e where
  r' = removeLastFragment e.editedRoute.route
  s' = maybe SelectInitial SelectNext $ lastStop r'

type CreateMap = Tuple (RouteIdMap StopId) (RouteIdMap (Pair StopId))

createMap :: Editor -> RoutesMap
createMap e = { selected: selectedStops e
  , perimeterRouteIds: fst result
  , roadRouteIds: snd result
  } where
  addRouteId :: forall k. Ord k => RouteId -> RouteIdMap k -> k -> RouteIdMap k
  addRouteId c cm k = M.insert k (S.insert c $ fromMaybe S.empty $ M.lookup k cm) cm
  addStop c (Tuple stopRouteIds roadStopIds) s = Tuple (addRouteId c stopRouteIds s) roadStopIds
  addRouteFragment :: RouteId -> CreateMap -> RouteFragment ->  CreateMap
  addRouteFragment routeId (Tuple stopRouteIds roadStopIds) rf = let
    stops = [lastFragmentStop rf, firstFragmentStop rf]
    stopRouteIds' = foldl (addRouteId routeId) stopRouteIds stops
    roadStopIds' = foldl (addRouteId routeId) roadStopIds (fragmentRoads rf)
    in Tuple stopRouteIds' roadStopIds'
  addRoute :: CreateMap -> Route -> CreateMap
  addRoute cm r = foldl (addRouteFragment r.routeId) cm r.fragments
  addRoutes cm = foldl addRoute cm e.routes
  addEditedRoute cm@(Tuple stopRouteIds roadStopIds) = let
    c = e.editedRoute.route.routeId
    cm' = case e.editedRoute.state of
      FirstStopCandidate s -> addStop c cm s
      FirstStopSelected s  -> addStop c cm s
      FragmentCandidate _ _ _ rf -> addRouteFragment c cm rf
      _ -> cm
    in addRoute cm' e.editedRoute.route
  result = addRoutes <<< addEditedRoute $ Tuple M.empty M.empty
  
selectedStops :: Editor -> S.Set StopId
selectedStops e =
  case e.editedRoute.state of
    FirstStopCandidate s        -> S.singleton s
    FirstStopSelected s         -> S.singleton s
    FragmentCandidate _ s1 s2 _ -> S.fromFoldable [ s1, s2 ]
    SelectNext last             -> S.singleton last
    _                           -> S.empty
