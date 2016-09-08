module View.Assignment where

import Pixi
import City (City)
import Data.Function.Uncurried (runFn0)
import Prelude

setup :: forall t. City -> PixiChEff t Graphics
setup c = let gfx = runFn0 newGraphics in pure gfx

draw :: forall t. Graphics -> PixiEff t Unit
draw gfx = pure unit
