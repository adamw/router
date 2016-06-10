module View.EditorControl
  ( setup
  , draw
  ) where

import City(City, residents, businesses)
import Control.Alt
import Data.Coords
import Data.Foldable
import Data.Function
import Data.Int
import Data.Maybe
import Data.Sequence
import Editor
import Pixi
import Prelude
import Route
import Signal.Channel
import View.Dimensions
import View.Route as RouteView
import View.Actions(Action(Complete))

setup :: forall t. Number -> PixiEff t Graphics
setup height = do
  cntr <- newGraphics
  _    <- withGraphics [ lineStyle (Width 1.0) boxBorderColor opaque
                       , moveTo origin2D
                       , lineTo { x: 0.0, y: height }  
                       ] cntr
  return cntr

draw :: forall t. Channel Action -> Graphics -> Editor -> PixiChEff t Unit
draw ch cntr editor = do
  _      <- removeAllFromContainer cntr
  banner <- newTextWithStyle "Route Planner" defaultTextStyle
  state  <- drawEditorState editor.city editor.editedRoute.state
  editedBanner <- newTextWithStyle "Edited route:" smallTextStyle
  editedBox <- drawEditedRouteBox ch editor.editedRoute
  allBanner <- newTextWithStyle "All routes:" smallTextStyle
  _      <- addToContainerAt banner { x: boxTxtOffset, y: 0.0 } cntr
  _      <- addToContainerAt state { x: 0.0, y: 30.0 } cntr
  _      <- addToContainerAt editedBanner { x: boxTxtOffset, y: 30.0+boxH } cntr
  _      <- addToContainerAt editedBox { x: 0.0, y: 50.0+boxH } cntr
  _      <- addToContainerAt allBanner { x: boxTxtOffset, y: 50.0+2.0*boxH } cntr
  const unit <$> foldl addRouteBox (return $ boxH*2.0+70.0) editor.routes where
    addRouteBox my r = do
      y <- my
      routeBox <- drawRouteBox r Nothing
      _ <- addToContainerAt routeBox { x: 0.0, y: y } cntr
      return (y + boxH)

boxBorderColor = Color 0x555555

drawEditorState city state = let
  editorMsg SelectFirst            = "Select first stop"
  editorMsg (FirstStopCandidate _) = "Tap to select first stop"
  editorMsg (FirstStopSelected _)  = "First stop selected"
  editorMsg (FragmentCandidate _ _ _ _) = "Tap to add route fragment"
  editorMsg (SelectNext s)         = "Select next stop"
  showStop (FirstStopCandidate s) = Just s
  showStop (FirstStopSelected s)  = Just s
  showStop (FragmentCandidate _ _ s _) = Just s
  showStop (SelectNext s)         = Just s
  showStop _                      = Nothing
  stopMsg s = (show s)
              ++ ", residents: "
              ++ (show $ residents s city)
              ++ ", businesses: "
              ++ (show $ businesses s city)
  in do    
    editorText <- newTextWithStyle (editorMsg state) smallTextStyle
    stopText   <- newTextWithStyle (fromMaybe "" $ stopMsg <$> showStop state) smallTextStyle
    cntr       <- newContainer
    _          <- addToContainerAt editorText { x: boxTxtOffset, y: box_1st_lineOffset } cntr
    _          <- addToContainerAt stopText   { x: boxTxtOffset, y: box_2nd_lineOffset } cntr
    return cntr


drawEditedRouteBox ch editedRoute = do
  editedBox   <- drawRouteBox editedRoute.route firstSelected
  completeBtn <- drawButton "✓" ch (Complete editedRoute.route.routeId)
  _           <- addToContainerAt completeBtn { x: boxW-boxH, y: 0.0 } editedBox
  return editedBox where
    firstSelected = case editedRoute.state of
      FirstStopSelected s       -> Just s
      FragmentCandidate _ s _ _ -> Just s
      _                         -> Nothing

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
  in do
  gfx <- newGraphics
  _   <- withGraphics [ lineStyle (Width 1.0) boxBorderColor opaque
                      , moveTo { x: 0.0, y: boxH }
                      , lineTo { x: boxW, y: boxH }
                      , beginFill routeColor opaque 
                      , lineStyle (Width 1.0) routeColor opaque 
                      , drawRect { x: 1.0, y: 1.0 } (boxH-1.0) (boxH-1.0) 
                      , endFill
                      ] gfx
  fromToText <- newTextWithStyle (fromStopName ++ " - " ++ toStopName) smallTextStyle
  let textXOffset = boxH+boxTxtOffset
  _   <- addToContainerAt fromToText { x: textXOffset, y: boxTxtOffset } gfx
  stopCountText <- newTextWithStyle ("Stops: " ++ (show stopCount)) smallTextStyle
  _   <- addToContainerAt stopCountText { x: textXOffset, y: box_2nd_lineOffset } gfx
  return gfx
  
drawButton label ch action = do
  gfx <- newGraphics
  _   <- withGraphics [ lineStyle (Width 1.0) (Color 0x000000) opaque
                      , drawRect origin2D boxH boxH 
                      ] gfx
  txt <-        newTextWithStyle label defaultTextStyle
  _   <- runFn3 setAnchor 0.5 0.5 txt
  _   <-        addToContainerAt txt { x: boxH/2.0, y: boxH/2.0 } gfx
  ha  <- runFn3 newRectangle origin2D boxH boxH
  _   <-        newButton ha gfx
  _   <-        onMouseDown ch action gfx
  return gfx

