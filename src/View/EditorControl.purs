module View.EditorControl
  ( draw
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
import View.Route as RouteView
import View.Actions(Action(Complete))

draw :: forall t. Channel Action -> Container -> Editor -> PixiChEff t Unit
draw ch cntr editor = do
  _      <- runFn1 removeAllFromContainer cntr
  banner <- newTextWithStyle "Route Planner" defaultTextStyle
  _      <- addToContainerAt banner { x: 0.0, y: 0.0 } cntr
  editedBanner <- newTextWithStyle "Edited route:" smallTextStyle
  _      <- addToContainerAt editedBanner { x: 0.0, y: 30.0 } cntr
  editedBox <- drawRouteBox editor.editedRoute.route firstSelected
  completeBtn <- drawButton "✓" ch (Complete editor.editedRoute.route.routeId)
  _      <- addToContainerAt completeBtn { x: 130.0, y: 0.0 } editedBox
  _      <- addToContainerAt editedBox { x: 0.0, y: 50.0 } cntr
  state  <- drawEditorState editor.city editor.editedRoute.state
  _      <- addToContainerAt state { x: 0.0, y: 50.0+routeBoxHeight } cntr
  const unit <$> foldl addRouteBox (return $ routeBoxHeight*2.0+50.0) editor.routes where
    firstSelected = case editor.editedRoute.state of
      FirstStopSelected s       -> Just s
      FragmentCandidate _ s _ _ -> Just s
      _                         -> Nothing
    addRouteBox my r = do
      y <- my
      routeBox <- drawRouteBox r Nothing
      _ <- addToContainerAt routeBox { x: 0.0, y: y } cntr
      return (y + routeBoxHeight)

routeBoxHeight = 30.0
smallTextHeight = 12.0
routeBoxTextOffset = 2.0
secondLineOffset = routeBoxTextOffset+smallTextHeight+routeBoxTextOffset

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
    cntr        <- runFn0 newContainer
    _          <- addToContainerAt editorText { x: 0.0, y: routeBoxTextOffset } cntr
    _          <- addToContainerAt stopText { x: 0.0, y: secondLineOffset } cntr
    return cntr

drawRouteBox route firstSelected = let
  routeColor = RouteView.color route.routeId
  fromStop = firstStop route <|> firstSelected
  toStop = lastStop route
  fromStopName = fromMaybe "?" $ show <$> fromStop
  toStopName = fromMaybe "?" $ show <$> toStop
  stopCount = let
    base = length route.fragments
    fromExtra = fromMaybe 0 $ const 1 <$> firstSelected
    toExtra = if fromStop == toStop then 0 else 1
    in if base == 0 then fromExtra else base+toExtra
  in do
  gfx <- runFn0 newGraphics
  _   <-        beginFill routeColor opaque gfx
  _   <-        lineStyle (Width 1.0) routeColor opaque gfx
  _   <-        drawRect origin2D routeBoxHeight routeBoxHeight gfx
  _   <-        endFill gfx
  fromToText <- newTextWithStyle (fromStopName ++ " - " ++ toStopName) smallTextStyle
  let textXOffset = routeBoxHeight+routeBoxTextOffset
  _   <- addToContainerAt fromToText { x: textXOffset, y: routeBoxTextOffset } gfx
  stopCountText <- newTextWithStyle ("Stops: " ++ (show stopCount)) smallTextStyle
  _   <- addToContainerAt stopCountText { x: textXOffset, y: secondLineOffset } gfx
  return gfx
  
drawButton label ch action = do
  gfx <- runFn0 newGraphics
  txt <-        newTextWithStyle label defaultTextStyle
  _   <-        runFn3 setAnchor 0.5 0.5 txt
  _   <-        addToContainerAt txt { x: routeBoxHeight/2.0, y: routeBoxHeight/2.0 } gfx
  _   <- runFn2 setInteractive true gfx
  _   <- runFn2 setButtonMode true gfx
  ha  <- runFn3 newRectangle origin2D routeBoxHeight routeBoxHeight
  _   <- runFn2 setHitArea ha gfx
  _   <-        onMouseDown ch action gfx
  _   <-        lineStyle (Width 1.0) (Color 0x000000) opaque gfx
  _   <-        drawRect origin2D routeBoxHeight routeBoxHeight gfx
  return gfx

