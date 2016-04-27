module Editor
  ( EditorState()
  , EditedRoute
  , Editor
  , ColorMap
  , RoutesMap
  , emptyEditor
  , selectStop
  , candidateStop
  , removeLastStop
  , createMap
  ) where

import StopRoute
import City

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.Tuple
import Data.Foldable
import Data.Pair

import Data.Sequence as SQ

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

type ColorMap k = M.Map k (S.Set Color)

type RoutesMap =
  { stopsCoords :: M.Map StopId Coords
  , selected :: S.Set StopId
  , perimeterColors :: ColorMap StopId
  , lineColors :: ColorMap (Pair StopId)
  -- editor state - so that changes can be detected & msgs displayed? or a user msg buffer?
  }

emptyEditor :: City -> Editor
emptyEditor c = { city: c, routes: SQ.empty, editedRoute: er } where
  er = { route: emptyRoute firstColor, state: SelectInitial }

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
    Nothing                           -> whenRouteEmpty s e
    Just last | firstStop r == Just s -> whenChosenIsFirst last s e
    Just _ | routeContains s r        -> e
    Just last                         -> whenChosenIsNew last s e

finishRoute :: Editor -> Editor
finishRoute e@{ routes = rs } = e { routes = rs', editedRoute = er' } where
  rs' = SQ.cons e.editedRoute.route rs
  color' = nextColor e.editedRoute.route.color
  er' = { route: emptyRoute color', state: SelectInitial }

removeLastStop :: Editor -> Editor
removeLastStop e = setState s' <<< setEditedRoute r' $ e where
  r' = removeLastFragment e.editedRoute.route
  s' = if SQ.length r'.fragments == 0 then SelectInitial else SelectNext

type CreateMap = Tuple (ColorMap StopId) (ColorMap (Pair StopId))

createMap :: Editor -> RoutesMap
createMap e = { stopsCoords: stopsCoords e.city
  , selected: selectedStops e
  , perimeterColors: fst result
  , lineColors: snd result
  } where
  addColor :: forall k. Ord k => Color -> ColorMap k -> k -> ColorMap k
  addColor c cm k = M.insert k (S.insert c $ fromMaybe S.empty $ M.lookup k cm) cm
  addStop c (Tuple stopColors roadColors) s = Tuple (addColor c stopColors s) roadColors
  addRouteFragment :: Color -> CreateMap -> RouteFragment ->  CreateMap
  addRouteFragment color (Tuple stopColors roadColors) rf = let
    stopColors' = foldl (addColor color) stopColors rf
    roadColors' = foldl (addColor color) roadColors (roads rf)
    in Tuple stopColors' roadColors'
  addRoute :: CreateMap -> Route -> CreateMap
  addRoute cm r = foldl (addRouteFragment r.color) cm r.fragments
  addRoutes cm = foldl addRoute cm e.routes
  addEditedRoute cm@(Tuple stopColors roadColors) = let
    c = e.editedRoute.route.color
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
