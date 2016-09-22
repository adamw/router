module View.Editor 
  ( setup
  , draw
  ) where

import Prelude
import Pixi
import ChSend
import Data.Map as M
import View.RoutesMap as RoutesMapView
import City (City, stopsCoords)
import Control.Apply ((*>))
import Data.Coords (origin2D)
import Data.Foldable (foldl)
import Data.Function.Uncurried (runFn0, runFn2)
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))
import RoutesMap (RoutesMap)
import View.Actions (RouteMapAction(Hover, Click), Action(..))

setup :: forall t. ChSend Action -> City -> PixiChEff t { btns :: Container, gfx :: Graphics }
setup ch city = let gfx = runFn0 newGraphics in do
  btns <- setupButtons (RouteMapAction >$< ch) city
  pure { btns: btns, gfx: gfx }
  
setupButtons :: forall t. ChSend RouteMapAction -> City -> PixiChEff t Container
setupButtons ch city = let btns = runFn0 newContainer in do
  _    <- foldl (setupButton ch btns) (pure unit) (M.toList (stopsCoords city))
  pure btns

setupButton ch btns acc (Tuple stopId stopCoords) = acc *> let g = runFn0 newGraphics in do
  ha <- runFn2 newCircle origin2D 15.0
  _  <-        newButton ha g
  _  <-        addToContainerAt g stopCoords  btns
  _  <-        onMouseDown ch (Click stopId) g
  _  <-        onMouseHover ch (Hover (Just stopId)) (Hover Nothing) g
  pure unit

draw :: forall t. Graphics -> City -> RoutesMap -> PixiEff t Unit
draw = RoutesMapView.draw
