module RouteEditor where

import StopRoute
import City

import Data.Maybe
import Data.Set as S
import Data.Map as M
import Data.Tuple

type EditedRoute =
  { route :: Route
  , nextCandidate :: Maybe StopId
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
