module View.ModePicker
  ( ViewState
  , setup
  , draw
  ) where

import Pixi
import Prelude
import Math as Math
import ChSend (ChSend)
import Control.Monad.Eff (foreachE)
import DOM.HTML.HTMLButtonElement (disabled)
import Data.Array (reverse, mapWithIndex, length)
import Data.Coords (origin2D)
import Data.Function.Uncurried (runFn0)
import Data.Int (toNumber)
import Data.Maybe (Maybe(Just))
import Data.Tuple (Tuple(Tuple))
import Modes (modeEnabled, allModes, Mode)
import View.Actions (Action(SwitchToMode))
import View.Buttons (drawButton)

type ViewState = { width :: Number
                 , height :: Number
                 , gfx :: Graphics
                 }

setup :: Number -> Number -> ViewState
setup w h = { width: w, height: h, gfx: runFn0 newGraphics }

draw :: forall r. Mode -> ChSend Action -> ViewState -> PixiChEff r Unit
draw current ch vs = removeAllFromContainer vs.gfx >>= \_ -> let
  btnW = vs.width / (toNumber (length allModes))
  btnH = vs.height
  drawBtns = (\m -> drawModeSwitchButton m (m == current) btnW btnH (modeEnabled current m) (show m) ch) <$> allModes
  drawBtnsWithStartX = mapWithIndex (\i -> \b -> Tuple b ((toNumber i)*btnW)) drawBtns
  addBtn (Tuple drawBtn x) = drawBtn >>= \btn -> addToContainerAt btn { x: x, y: 0.0 } vs.gfx
  in foreachE (reverse drawBtnsWithStartX) addBtn

modePickerBgColor = Color 0x666666
modePickerSelectedBgColor = Color 0x888888

enabledTextStyle  = defaultTextStyle { fontWeight = "normal", fill = 0xEEEEEE }
disabledTextStyle = defaultTextStyle { fontWeight = "normal", fill = "gray" }

drawModeSwitchButton :: forall r. Mode -> Boolean -> Number -> Number -> Boolean -> String -> ChSend Action -> PixiChEff r Graphics
drawModeSwitchButton m isCurrent w h enabled label ch = let
  gfx = runFn0 newGraphics
  tooltip = (Just $ "Switch to step: " <> label)
  action = (SwitchToMode m)
  bgColor = if isCurrent then modePickerSelectedBgColor else modePickerBgColor
  in do
  _  <- withGraphics [ beginFill bgColor opaque
                     , lineStyle (Width 1.0) black opaque
                     , moveTo origin2D
                     , lineTo { x: w, y: 0.0 }
                     , lineTo { x: w + h/2.0*(Math.tan 0.52), y: h/2.0 } -- ~30 degrees
                     , lineTo { x: w, y: h }
                     , lineTo { x: 0.0, y: h }
                     , endFill
                     ] gfx
  _ <- if enabled
       then drawButton w h label tooltip ch action enabledTextStyle gfx
       else do
         txt <- newTextWithStyle label disabledTextStyle
         _   <- addToContainerAtMiddle txt w h gfx
         pure unit
  pure gfx
  
