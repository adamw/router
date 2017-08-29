module View.AssignmentControl
  ( draw
  ) where

import Assignment
import Prelude
import ChSend
import Pixi
import Pixi.Packer
import Data.Map as M
import View.WithControl (boxBorderColor)
import View.Route as RouteView
import Data.Coords (origin2D)
import Data.Foldable (traverse_)
import Data.Function.Uncurried (runFn0)
import Data.Maybe (fromMaybe, Maybe(Just))
import Data.Array (length)
import View.Actions (AssignmentAction(AddBus, RemoveBus), Action(AssignmentAction))
import View.Buttons (drawStdSqButton)
import View.Dimensions (boxTxtOffset, boxW, boxH)

draw :: forall t. ChSend Action -> Graphics -> Assignment -> PixiChEff t Unit
draw ch cntr assignment = removeAllFromContainer cntr >>= \_ -> let
  banner       = newTextWithStyle "Assign buses" defaultTextStyle
  packed       = do
    _ <- vpack boxTxtOffset 30.0 banner
    _ <- traverse_ (vpack 0.0 boxH <<< drawAssignToRouteBox ch assignment) assignment.routes
    pure unit
  in const unit <$> execVPacker cntr origin2D packed

drawAssignToRouteBox ch assignment route = let
  count      = fromMaybe 0 $ M.lookup route.routeId assignment.buses
  routeColor = RouteView.color route.routeId
  stopCount  = length route.fragments
  gfx        = runFn0 newGraphics
  in do
    removeBtn     <- if count > 0
                     then drawStdSqButton "-" (Just "Remove bus") ch (AssignmentAction (RemoveBus route.routeId))
                     else pure $ runFn0 newGraphics
    addBtn        <- if assignment.available > 0
                     then drawStdSqButton "+" (Just "Add bus") ch (AssignmentAction (AddBus route.routeId))  
                     else pure $ runFn0 newGraphics                         
    _             <- withGraphics [ lineStyle (Width 1.0) boxBorderColor opaque
                                  , moveTo { x: 0.0,  y: boxH }
                                  , lineTo { x: boxW, y: boxH }
                                  , beginFill routeColor opaque 
                                  , lineStyle (Width 1.0) routeColor opaque 
                                  , drawRect { x: boxH+1.0, y: 1.0 } (boxW-boxH-boxH-1.0) (boxH-2.0) 
                                  , endFill
                                  ] gfx
    busCountText  <- newTextWithStyle ((show count) <> " buses") smallTextStyle
    _             <- setMiddleAnchor busCountText                 
    stopCountText <- newTextWithStyle ((show stopCount) <> " stops") smallTextStyle
    _             <- setMiddleAnchor stopCountText                 
    _             <- addToContainerAt removeBtn { x: 0.0,       y: 0.0 } gfx
    _             <- addToContainerAt busCountText { x: boxW/2.0, y: boxH*0.25 } gfx                   
    _             <- addToContainerAt addBtn    { x: boxW-boxH, y: 0.0 } gfx     
    _             <- addToContainerAt stopCountText { x: boxW/2.0, y: boxH*0.75 } gfx                   
    pure gfx
