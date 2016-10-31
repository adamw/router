module View.Assignment where

import Pixi
import Prelude
import View.RoutesMap as RoutesMapView
import Assignment (Assignment)
import City (City)
import Data.Function.Uncurried (runFn0)

setup :: forall t. City -> PixiChEff t Graphics
setup c = let gfx = runFn0 newGraphics in pure gfx

draw :: forall t. Assignment -> Graphics -> PixiEff t Unit
draw a gfx = RoutesMapView.draw gfx a.city a.routesMap 
