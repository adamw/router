module View.EditorControl
  ( setup
  , draw
  ) where

import Control.Alt
import Data.Coords
import Data.Foldable
import Data.Maybe
import Data.Sequence
import Editor
import Pixi
import Pixi.Packer
import Prelude
import Route
import ChSend
import View.Buttons
import View.Dimensions
import View.Route as RouteView
import Data.Function.Uncurried (runFn0)
import View.Actions (Action(EditorAction), EditorAction(CompleteRoute, RemoveLastStop, RemoveRoute, EditRoute))

setup :: forall t. Number -> PixiEff t Graphics
setup height = let cntr = runFn0 newGraphics in do
  _    <- withGraphics [ lineStyle (Width 1.0) boxBorderColor opaque
                       , moveTo origin2D
                       , lineTo { x: 0.0, y: height }  
                       ] cntr
  pure cntr

draw :: forall t. ChSend Action -> Graphics -> Editor -> PixiChEff t Unit
draw ch cntr editor = removeAllFromContainer cntr >>= \_ -> let
  banner       = newTextWithStyle "Route Planner" defaultTextStyle
  editedBanner = newTextWithStyle "Edited route:" smallTextStyle
  editedBox    = drawEditedRouteBox ch editor.editedRoute
  allBanner    = newTextWithStyle "All routes:" smallTextStyle
  packed       = do
    _ <- vpack boxTxtOffset 30.0 banner
    _ <- vpack boxTxtOffset 20.0 editedBanner
    _ <- vpack 0.0 boxH editedBox
    _ <- vpack boxTxtOffset 20.0 allBanner
    _ <- traverse_ (vpack 0.0 boxH <<< drawDoneRouteBox ch) editor.routes
    pure unit
  in const unit <$> execVPacker cntr origin2D packed

boxBorderColor = Color 0x555555

drawEditedRouteBox ch editedRoute = do
  editedBox     <- drawRouteBox editedRoute.route firstSelected
  completeBtn   <- drawSqButton "✓" (Just "Complete route") ch (EditorAction CompleteRoute)
  removeLastBtn <- drawSqButton "⎌" (Just "Remove last stop") ch (EditorAction RemoveLastStop)
  _             <- addToContainerAt completeBtn   { x: boxW-boxH, y: 0.0 } editedBox
  _             <- addToContainerAt removeLastBtn { x: boxW-2.0*boxH, y: 0.0 } editedBox
  pure editedBox where
    firstSelected = case editedRoute.state of
      FirstStopSelected s       -> Just s
      FragmentCandidate _ s _ _ -> Just s
      _                         -> Nothing

drawDoneRouteBox ch route = do
  routeBox      <- drawRouteBox route Nothing
  removeBtn     <- drawSqButton "✕" (Just "Remove route") ch (EditorAction (RemoveRoute route.routeId))
  editBtn       <- drawSqButton "✐" (Just "Edit route") ch (EditorAction (EditRoute route.routeId))  
  _             <- addToContainerAt removeBtn { x: boxW-boxH, y: 0.0 } routeBox
  _             <- addToContainerAt editBtn   { x: boxW-2.0*boxH, y: 0.0 } routeBox
  pure routeBox
  
drawRouteBox route firstSelected = let
  routeColor   = RouteView.color route.routeId
  fromStop     = firstStop route <|> firstSelected
  toStop       = lastStop route
  fromStopName = fromMaybe "?" $ show <$> fromStop
  toStopName   = fromMaybe "?" $ show <$> toStop
  stopCount    = let
    base      = length route.fragments
    fromExtra = fromMaybe 0 $ const 1 <$> firstSelected
    toExtra   = if fromStop == toStop then 0 else 1
    in if base == 0 then fromExtra else base+toExtra
  gfx = runFn0 newGraphics
  in do
  _   <- withGraphics [ lineStyle (Width 1.0) boxBorderColor opaque
                      , moveTo { x: 0.0, y: boxH }
                      , lineTo { x: boxW, y: boxH }
                      , beginFill routeColor opaque 
                      , lineStyle (Width 1.0) routeColor opaque 
                      , drawRect { x: 1.0, y: 1.0 } (boxH-1.0) (boxH-1.0) 
                      , endFill
                      ] gfx
  fromToText <- newTextWithStyle (fromStopName <> " - " <> toStopName) smallTextStyle
  let textXOffset = boxH+boxTxtOffset
  _   <- addToContainerAt fromToText { x: textXOffset, y: boxTxtOffset } gfx
  stopCountText <- newTextWithStyle ("Stops: " <> (show stopCount)) smallTextStyle
  _   <- addToContainerAt stopCountText { x: textXOffset, y: box_2nd_lineOffset } gfx
  pure gfx
  
