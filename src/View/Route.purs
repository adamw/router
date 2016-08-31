module View.Route
  ( color
  ) where

import Prelude
import Data.Maybe(fromMaybe)
import Data.Array(index)
import Route(RouteId(RouteId))
import Pixi(Color(Color))

colors :: Array Int
colors = [ 0xFF0000, 0x00FF00, 0x0000FF, 0x880000, 0x008800, 0x000088 ]

color :: RouteId -> Color
color (RouteId rid) = Color $ fromMaybe 0x000000 $ index colors (rid-1)

