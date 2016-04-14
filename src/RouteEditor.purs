module RouteEditor where

import StopRoute
import City

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.Tuple

import Data.Sequence as SQ
import Data.Sequence.NonEmpty as NE

{-
* when hovering over the initial stop, the next candidate is a single-stop fragment
* when initial stop is clicked, the route contains a single single-stop fragment and there's no candidate
* when the next stop is hovered, there's a candidate with the shortest path
* when a stop is clicked, this is moved to the route and there's no candidate
-}

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
  , routes :: Array Route
  , editedRoute :: EditedRoute
  }

type EditorView =
  { stopsCoords :: M.Map StopId Coords
  , selected :: S.Set StopId
  , perimeterColors :: M.Map StopId (Array Color)
  , lineColors :: M.Map (Tuple StopId StopId) (Array Color)
  -- editor state - so that changes can be detected & msgs displayed? or a user msg buffer?
  }

clicked :: StopId -> Editor -> Editor
clicked s e = e

hovered :: Maybe StopId -> Editor -> Editor
hovered s e = e

createView :: Editor -> EditorView
createView s = { stopsCoords = stopsCoords s.city
  , selected = fromFoldable $ selectedStop s
  , perimeterColors = M.empty
  , lineColors = M.empty
  }

selectedStop :: Editor -> Maybe StopId
selectedStop s =
  sel s.editedRoute.state where
  sel (FirstStopCandidate s) = Just s
  sel (FirstStopSelected s) = Just s
  sel (FragmentCandidate f) = Just (lastFragmentStop f)
  sel _ = Nothing  
