module View.Tooltip (TooltipState, TooltipViewState, setup, draw) where

import Pixi
import Prelude
import Data.Coords (origin2D)
import Data.Function.Uncurried (runFn0, runFn2)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Tuple (Tuple(Tuple))

type TooltipState = Maybe String

type TooltipViewState =
  { txt :: Text
  , gfx :: Graphics
  }

tooltipBgColor = Color 0x666666

setup :: forall r. Number -> Number -> PixiEff r (Tuple TooltipState TooltipViewState)
setup width height = let gfx = runFn0 newGraphics in do
  _   <- withGraphics [ lineStyle (Width 1.0) tooltipBgColor opaque
                      , beginFill tooltipBgColor opaque
                      , drawRect origin2D width height
                      , endFill
                      ] gfx
  txt <- newTextWithStyle "" (smallTextStyle { fill = "white" })
  _   <- setMiddleAnchor txt
  _   <- addToContainerAt txt { x: width/2.0, y: height/2.0 } gfx
  pure $ Tuple Nothing { txt: txt, gfx: gfx } 

draw :: forall r. TooltipState -> TooltipViewState -> PixiEff r Unit
draw (Just tooltip) view = runFn2 setText tooltip view.txt
draw Nothing        view = runFn2 setText ""      view.txt

