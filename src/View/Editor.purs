module View.Editor 
  ( setup
  , draw
  , EditorView
  ) where

import Prelude
import Editor
import Pixi
import View.Actions
import Data.Function
import Data.Foldable
import Data.Coords
import Signal.Channel
import Data.Map as M
import Data.Set as S
import City (stopsCoords, City, roads)
import Control.Apply ((*>))
import Data.Array (index)
import Data.Int (toNumber)
import Data.List (List(Nil), (:))
import Data.Maybe (fromMaybe)
import Data.Pair (Pair(Pair))
import Data.Tuple (fst, Tuple(Tuple))
import Route (RouteId(RouteId))

type EditorView =
  { btnsLayer :: Container
  , gfxLayer :: Graphics
  }

setup :: forall t. Channel Action -> City -> RoutesMap -> PixiChEff t EditorView
setup ch city rm = do
  gfx  <- runFn0 newGraphics
  _    <- draw gfx city rm
  btns <- setupButtons ch city
  return { btnsLayer: btns, gfxLayer: gfx }
  
setupButtons :: forall t. Channel Action -> City -> PixiChEff t Container
setupButtons ch city = do
  btns <- runFn0 newContainer
  _    <- foldl (setupButton ch btns) (return unit) (M.toList (stopsCoords city))
  return btns

setupButton ch btns acc (Tuple stopId stopCoords) = acc *> do
  g  <- runFn0 newGraphics
  _  <- runFn2 setInteractive true g
  _  <- runFn2 setButtonMode true g
  ha <- runFn2 newCircle origin2D 15.0
  _  <- runFn2 setHitArea ha g
  _  <- runFn2 setPosition stopCoords g
  _  <- runFn2 addToContainer g btns
  _  <-        onMouseDown ch (Click stopId) g
  return unit

draw :: forall t. Graphics -> City -> RoutesMap -> PixiEff t Unit
draw gfx city rm = do
  _ <- runFn1 removeAllFromContainer gfx
  _ <- runFn1 clear gfx
  _ <-        foldl (drawRoadRoutes gfx city) (return unit) (M.toList rm.roadRouteIds)
  _ <-        drawRoads gfx city
  _ <-        foldl (drawButton gfx rm) (return unit) (M.toList (stopsCoords city))
  return unit

buttonRadius = 15.0
  
drawButton btns rm acc (Tuple stopId stopCoords) = let
  inside g = do
    _ <- runFn3 beginFill (Color 0x4679BD) opaque g
    _ <- runFn4 lineStyle (Width 2.0) (Color 0x4679BD) opaque g
    _ <- runFn3 drawCircle origin2D buttonRadius g
    _ <- runFn1 endFill g
    return unit
  outsideIfSelected g = if S.member stopId rm.selected
    then do
      _ <- runFn4 lineStyle (Width 5.0) (Color 0xcfdc00) opaque g
      _ <- runFn3 drawCircle origin2D 19.0 g
      return unit
    else return unit
  in acc *> do
    g <- runFn0 newGraphics
    _ <- inside g
    _ <- outsideIfSelected g
    _ <- runFn2 setPosition stopCoords g
    _ <- runFn2 addToContainer g btns
    return unit

drawRoads gfx city = let
  drawRoad (Pair s1 s2) = withCoords city drw s1 s2 where
    drw c1 c2 = do
      _ <- runFn4 lineStyle (Width 3.0) (Color 0x111111) opaque gfx
      _ <- runFn2 moveTo c1 gfx
      _ <- runFn2 lineTo c2 gfx
      return unit
  in foldl (\acc r -> acc *> (drawRoad r)) (return unit) (roads city)    

createRoadRoutesRect routes = let
  routeCount  = S.size routes
  routeHeight = Math.min 5.0 (buttonRadius*2.0/(toNumber routeCount))
  startingY   = negate (toNumber routeCount)/2.0*routeHeight
  drawRoute (Tuple r y) g = do
    let c = color r
    _ <- runFn3 beginFill c opaque g
    _ <- runFn4 lineStyle (Width 0.0) c opaque g
    _ <- runFn4 drawRect { x: 0.0, y: y } 10.0 routeHeight g
    _ <- runFn1 endFill g
    return unit
  routesWithY :: List (Tuple RouteId Number)
  routesWithY = let
    addRouteWithY r (Tuple result y) = Tuple ((Tuple r y) : result) (y + routeHeight) 
    in fst $ foldr addRouteWithY (Tuple Nil startingY) (S.toList routes)
  drawRoutes g = foldl (\acc t -> acc *> (drawRoute t g)) (return unit) routesWithY
  in do
    g <- runFn0 newGraphics
    _ <- drawRoutes g
    return g

drawRoadRoutes gfx city acc (Tuple (Pair s1 s2) routes) =
  acc *> withCoords city drw s1 s2 where
  drw c1 c2 = do
    rr <- createRoadRoutesRect routes
    _ <- runFn2 setWidth (distance c1 c2) rr
    let angle = Math.atan2 (c2.y-c1.y) (c2.x-c1.x)
    _ <- runFn2 setRotation angle rr
    _ <- runFn2 setPosition c1 rr
    _ <- runFn2 addToContainer rr gfx
    return unit

withCoords city f s1 s2 = fromMaybe (return unit) $ do
  c1 <- M.lookup s1 (stopsCoords city)
  c2 <- M.lookup s2 (stopsCoords city)
  return $ f c1 c2

colors = [ 0xFF0000, 0x00FF00, 0x0000FF, 0x880000, 0x008800, 0x000088 ]

color (RouteId rid) = Color $ fromMaybe 0x000000 $ index colors (rid-1)
