module RouteEditor
  ( EditorState()
  , EditedRoute
  , Editor
  , ColorMap
  , EditorView
  , emptyEditor
  , clicked
  , hovered
  , removeLast
  , createView
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
import Data.Sequence.NonEmpty as NE

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

type EditorView =
  { stopsCoords :: M.Map StopId Coords
  , selected :: S.Set StopId
  , perimeterColors :: ColorMap StopId
  , lineColors :: ColorMap (Pair StopId)
  -- editor state - so that changes can be detected & msgs displayed? or a user msg buffer?
  }

emptyEditor :: City -> Editor
emptyEditor c = { city: c, routes: SQ.empty, editedRoute: er } where
  color = Color 1
  er = { route: emptyRoute color, state: SelectInitial }

setState       st e = e { editedRoute = e.editedRoute { state = st } }
setEditedRoute r  e = e { editedRoute = e.editedRoute { route = r  } }
                   
clicked :: StopId -> Editor -> Editor
clicked s e@{ editedRoute = { route = r } } =
  case lastStop r of
    Nothing                    -> setState (FirstStopSelected s) e
    Just _ | routeContains s r -> e
    Just last                  -> addFrom last
  where
    addFrom:: StopId -> Editor
    addFrom sFrom = let 
      r' = addFragment (City.routeFragment sFrom s e.city) r
      in setEditedRoute r' e
    
hovered :: Maybe StopId -> Editor -> Editor
hovered s e = e

removeLast :: Editor -> Editor
removeLast e = e

type CreateView = Tuple (ColorMap StopId) (ColorMap (Pair StopId))

createView :: Editor -> EditorView
createView e = { stopsCoords: stopsCoords e.city
  , selected: selectedStops e
  , perimeterColors: fst cv
  , lineColors: snd cv
  } where
  addColor :: forall k. Ord k => Color -> ColorMap k -> k -> ColorMap k
  addColor c cm k = M.insert k (S.insert c $ fromMaybe S.empty $ M.lookup k cm) cm
  addStop c (Tuple stopColors roadColors) s = Tuple (addColor c stopColors s) roadColors
  addRouteFragment :: Color -> CreateView -> RouteFragment ->  CreateView
  addRouteFragment color (Tuple stopColors roadColors) rf = let
    stopColors' = foldl (addColor color) stopColors rf
    roadColors' = foldl (addColor color) roadColors (roads rf)
    in Tuple stopColors' roadColors'
  addRoute :: CreateView -> Route -> CreateView
  addRoute cv r = foldl (addRouteFragment r.color) cv r.fragments
  addRoutes cv = foldl addRoute cv e.routes
  addEditedRoute cv@(Tuple stopColors roadColors) = let
    c = e.editedRoute.route.color
    cv' = case e.editedRoute.state of
      FirstStopCandidate s -> addStop c cv s
      FirstStopSelected s  -> addStop c cv s
      FragmentCandidate rf -> addRouteFragment c cv rf
      _ -> cv
    in addRoute cv' e.editedRoute.route
  cv = addRoutes <<< addEditedRoute $ Tuple M.empty M.empty
  
selectedStops :: Editor -> S.Set StopId
selectedStops e =
  case e.editedRoute.state of
    FirstStopCandidate s -> S.singleton s
    FirstStopSelected s  -> S.singleton s
    FragmentCandidate f  -> S.fromFoldable [ firstFragmentStop f, lastFragmentStop f ]
    SelectNext           -> S.fromFoldable $ lastStop e.editedRoute.route
    _                    -> S.empty
