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
import Data.Maybe
import Data.Map as M
import Data.Set as S
import City (stopsCoords, City, roads, residentFractions, businessFractions)
import Control.Apply ((*>))
import Data.Array (index)
import Data.Int (toNumber)
import Data.List (List(Nil), (:), zip, (..), length)
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
  _  <-        onMouseHover ch (Hover (Just stopId)) (Hover Nothing) g
  return unit

draw :: forall t. Graphics -> City -> RoutesMap -> PixiEff t Unit
draw gfx city rm = do
  _    <- runFn1 removeAllFromContainer gfx
  pop  <- runFn0 newGraphics
  rds  <- runFn0 newGraphics
  rts  <- runFn0 newGraphics
  btns <- runFn0 newGraphics
  _    <-        drawPopulation pop city (businessFractions city) (Color 0x8888FF) 0.0
  _    <-        drawPopulation pop city (residentFractions city) (Color 0x88FF88) Math.pi
  _    <-        foldl (drawRoadRoutes rts city) (return unit) (M.toList rm.roadRouteIds)
  _    <-        drawRoads rds city
  _    <-        foldl (drawPerimeter btns city) (return unit) (M.toList rm.perimeterRouteIds)
  _    <-        foldl (drawButton btns rm) (return unit) (M.toList (stopsCoords city))
  _    <- runFn2 addToContainer pop  gfx
  _    <- runFn2 addToContainer rds  gfx
  _    <- runFn2 addToContainer rts  gfx
  _    <- runFn2 addToContainer btns gfx
  return unit

buttonRadius = 15.0
extraPerimeterRadius = 2.0
maxExtraPopulationRadius = 15.0

drawButton btns rm acc (Tuple stopId stopCoords) = let
  inside g = do
    _ <- runFn3 beginFill (Color 0x4679BD) opaque g
    _ <- runFn4 lineStyle (Width 2.0) (Color 0x4679BD) opaque g
    _ <- runFn3 drawCircle stopCoords buttonRadius g
    _ <- runFn1 endFill g
    return unit
  outsideIfSelected g = if S.member stopId rm.selected
    then do
      _ <- runFn4 lineStyle (Width 5.0) (Color 0xcfdc00) opaque g
      _ <- runFn3 drawCircle stopCoords (buttonRadius + 5.0) g
      return unit
    else return unit
  in acc *> do
    _ <- inside btns
    _ <- outsideIfSelected btns
    return unit

drawRoads gfx city = let
  drawRoad (Pair s1 s2) = withCoords2 city drw s1 s2 where
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
  acc *> withCoords2 city drw s1 s2 where
  drw c1 c2 = do
    rr <- createRoadRoutesRect routes
    _ <- runFn2 setWidth (distance c1 c2) rr
    let angle = Math.atan2 (c2.y-c1.y) (c2.x-c1.x)
    _ <- runFn2 setRotation angle rr
    _ <- runFn2 setPosition c1 rr
    _ <- runFn2 addToContainer rr gfx
    return unit

drawPerimeter gfx city acc (Tuple s routes) =
  acc *> withCoords1 city drawAll s where
  routeCount = S.size routes
  routeArc = 2.0 * Math.pi / (toNumber routeCount)
  drawSingle c (Tuple r idx) = do
    _ <- runFn2 moveTo c gfx
    _ <- runFn4 lineStyle (Width 2.0) (color r) opaque gfx
    let startArc = routeArc*(toNumber idx)
    let endArc = routeArc*(toNumber $ idx+1)
    _ <- runFn5 arc c (buttonRadius+extraPerimeterRadius) startArc endArc gfx
    return unit
  routesWithIndex = zip (S.toList routes) (0 .. (routeCount - 1))
  drawAll c = foldl (\a t -> a *> (drawSingle c t)) (return unit) routesWithIndex

scaledFractions fracts = let
  max = fromMaybe 1.0 $ maximum $ M.values fracts
  in (\f -> f / max) <$> fracts

drawPopulation gfx city fracts color startArc = let
  scaled = scaledFractions fracts
  drawCoordPop acc fract coords = acc *> do
    _ <- runFn3 beginFill color opaque gfx
    _ <- runFn2 moveTo coords gfx
    _ <- runFn4 lineStyle (Width 0.0) color opaque gfx
    let radius = buttonRadius+extraPerimeterRadius+maxExtraPopulationRadius*fract
    _ <- runFn5 arc coords radius startArc (startArc+Math.pi) gfx
    _ <- runFn1 endFill gfx
    return unit
  drawStopPop acc (Tuple s fract) = withCoords1 city (drawCoordPop acc fract) s
  in foldl drawStopPop (return unit) (M.toList scaled)

withCoords1 city f s = fromMaybe (return unit) $ do
  c <- M.lookup s (stopsCoords city)
  return $ f c

withCoords2 city f s1 s2 = fromMaybe (return unit) $ do
  c1 <- M.lookup s1 (stopsCoords city)
  c2 <- M.lookup s2 (stopsCoords city)
  return $ f c1 c2

colors = [ 0xFF0000, 0x00FF00, 0x0000FF, 0x880000, 0x008800, 0x000088 ]

color (RouteId rid) = Color $ fromMaybe 0x000000 $ index colors (rid-1)
