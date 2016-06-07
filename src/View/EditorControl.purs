module View.EditorControl
  ( draw
  ) where

import City(City, residents, businesses)
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
import View.Actions(Action)

draw :: forall t. Container -> Editor -> PixiChEff t Unit
draw cnt editor = do
  _      <- runFn1 removeAllFromContainer cnt
  banner <- newTextWithStyle "Route Planner" defaultTextStyle
  _      <- addToContainerAt banner { x: 0.0, y: 20.0 } cnt
  state  <- drawEditorState editor.city editor.editedRoute.state
  _      <- addToContainerAt state { x: 0.0, y: bannerHeight } cnt
  const unit <$> foldl addRouteBox (return $ routeBoxHeight+bannerHeight) editor.routes where
    addRouteBox my r = do
      y <- my
      routeBox <- drawRoute r Nothing
      _ <- addToContainerAt routeBox { x: 0.0, y: y } cnt
      return (y + routeBoxHeight)

bannerHeight = 50.0
routeBoxHeight = 30.0
smallTextHeight = 12.0
routeBoxTextOffset = 2.0
secondLineOffset = routeBoxTextOffset+smallTextHeight+routeBoxTextOffset

drawEditorState city state = let
  editorMsg SelectInitial          = "Select first stop"
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
    cnt        <- runFn0 newContainer
    _          <- addToContainerAt editorText { x: 0.0, y: routeBoxTextOffset } cnt
    _          <- addToContainerAt stopText { x: 0.0, y: secondLineOffset } cnt
    return cnt
    
drawRoute r firstSelected = let
  routeColor = RouteView.color r.routeId
  fromStop = firstStop r
  toStop = lastStop r
  fromStopName = fromMaybe "?" $ show <$> fromStop
  toStopName = fromMaybe "?" $ show <$> toStop
  stopCount = let
    len = length r.fragments
    fromExtra = fromMaybe 0 $ const 1 <$> firstSelected
    toExtra = if fromStop == toStop then 0 else 1
    in if len == 0 then fromExtra else len+toExtra
  in do
  gfx <- runFn0 newGraphics
  _   <- runFn3 beginFill routeColor opaque gfx
  _   <- runFn4 lineStyle (Width 1.0) routeColor opaque gfx
  _   <- runFn4 drawRect origin2D routeBoxHeight routeBoxHeight gfx
  _   <- runFn1 endFill gfx
  fromToText <- newTextWithStyle (fromStopName ++ " - " ++ toStopName) smallTextStyle
  let textXOffset = routeBoxHeight+routeBoxTextOffset
  _   <- addToContainerAt fromToText { x: textXOffset, y: routeBoxTextOffset } gfx
  stopCountText <- newTextWithStyle ("Stops: " ++ (show stopCount)) smallTextStyle
  _   <- addToContainerAt stopCountText { x: textXOffset, y: secondLineOffset } gfx
  return gfx
  
