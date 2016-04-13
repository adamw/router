module RouteEditor where

import StopRoute
import City

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.Tuple

{-
* when hovering over the initial stop, the next candidate is a single-stop fragment
* when initial stop is clicked, the route contains a single single-stop fragment and there's no candidate
* when the next stop is howeverd, there's a candidate with the shortest path
* when a stop is clicked, this is moved to the route and there's no candidate
-}

type EditedRoute =
  { route :: Route
  , nextCandidate :: Maybe RouteFragment
  }

type EditorState =
  { city :: City
  , routes :: Array Route
  , editedRoute :: Maybe EditedRoute
  }

type EditorViewState =
  { stopsCoords :: M.Map StopId Coords
  , selected :: S.Set StopId
  , perimeterColors :: M.Map StopId (Array Color)
  , lineColors :: M.Map (Tuple StopId StopId) (Array Color)
  -- editor state - so that changes can be detected & msgs displayed? or a user msg buffer?
  }

createView :: EditorState -> EditorViewState
createView s = { stopsCoords = stopsCoords s.city
  , selected = fromFoldable $ selectedStop s
  , perimeterColors = M.empty
  , lineColors = M.empty
  }

selectedStop :: EditorState -> Maybe StopId
selectedStop s =  
  fromEditedRoute <$> s.editedRoute where
  fromEditedRoute { route: r, nextCandidate: Nothing } = lastStop r
  fromEditedRoute { nextCandidate: Just nc } = Just $ lastFragmentStop nc
  
