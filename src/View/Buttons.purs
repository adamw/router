module View.Buttons
  ( drawStdButton
  , drawStdSqButton
  , drawButton
  ) where

import Pixi
import Prelude
import ChSend
import View.Dimensions
import Data.Coords (origin2D)
import Data.Function.Uncurried (runFn3, runFn0)
import Data.Functor.Contravariant ((>$<))
import Data.Maybe (Maybe)
import View.Actions (TooltipAction(ClearTooltip, ShowTooltip), Action(TooltipAction))

drawStdSqButton :: forall r. String -> Maybe String -> ChSend Action -> Action -> PixiChEff r Graphics
drawStdSqButton = drawStdButton boxH

drawStdButton :: forall r. Number -> String -> Maybe String -> ChSend Action -> Action -> PixiChEff r Graphics
drawStdButton w label tooltip ch action = let
  gfx = runFn0 newGraphics
  h = boxH
  in do
  _   <- withGraphics [ lineStyle (Width 1.0) black opaque
                      , drawRect origin2D w h
                      ] gfx
  let drawn = drawButton w h label tooltip ch action defaultTextStyle gfx
  gfx <$ drawn

drawButton :: forall r a. Number -> Number -> String -> Maybe String -> ChSend Action -> Action -> { | a } -> Graphics -> PixiChEff r Unit
drawButton w h label tooltip ch action txtStyle gfx = do
    txt <-        newTextWithStyle label txtStyle
    _   <-        addToContainerAtMiddle txt w h gfx
    ha  <- runFn3 newRectangle origin2D w h
    _   <-        newButton ha gfx
    _   <-        onMouseDown ch action gfx
    _   <-        onMouseHover (TooltipAction >$< ch) (ShowTooltip tooltip) ClearTooltip gfx
    pure unit
