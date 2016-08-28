module View.Buttons
  ( drawButton
  , drawSqButton
  ) where

import Data.Coords (origin2D)
import Data.Function.Uncurried(runFn0, runFn3)
import Pixi
import Prelude
import Signal.Channel
import View.Dimensions

drawSqButton :: forall a r. String -> Channel a -> a -> PixiChEff r Graphics
drawSqButton = drawButton boxH

drawButton :: forall a r. Number -> String -> Channel a -> a -> PixiChEff r Graphics
drawButton width label ch action = let
  height = boxH
  gfx = runFn0 newGraphics
  in do
    _   <- withGraphics [ lineStyle (Width 1.0) (Color 0x000000) opaque
                        , drawRect origin2D width height
                        ] gfx
    txt <-        newTextWithStyle label defaultTextStyle
    _   <-        setMiddleAnchor txt
    _   <-        addToContainerAt txt { x: width/2.0, y: height/2.0 } gfx
    ha  <- runFn3 newRectangle origin2D width height
    _   <-        newButton ha gfx
    _   <-        onMouseDown ch action gfx
    pure gfx
