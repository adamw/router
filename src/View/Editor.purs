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
import View.Route (color) 

type EditorView =
  { btnsLayer :: Container
  , gfxLayer :: Graphics
  }

setup :: forall t. Channel Action -> City -> RoutesMap -> PixiChEff t EditorView
setup ch city rm = let gfx = runFn0 newGraphics in do
  btns <- setupButtons ch city
  pure { btnsLayer: btns, gfxLayer: gfx }
  
setupButtons :: forall t. Channel Action -> City -> PixiChEff t Container
setupButtons ch city = let btns = runFn0 newContainer in do
  _    <- foldl (setupButton ch btns) (return unit) (M.toList (stopsCoords city))
  pure btns

setupButton ch btns acc (Tuple stopId stopCoords) = acc *> let g = runFn0 newGraphics in do
  ha <- runFn2 newCircle origin2D 15.0
  _  <-        newButton ha g
  _  <-        addToContainerAt g stopCoords  btns
  _  <-        onMouseDown ch (Click stopId) g
  _  <-        onMouseHover ch (Hover (Just stopId)) (Hover Nothing) g
  pure unit

draw :: forall t. Graphics -> City -> RoutesMap -> PixiEff t Unit
draw gfx city rm = do
  _    <- removeAllFromContainer gfx
  let pop  = runFn0 newGraphics
  let rds  = runFn0 newGraphics
  let rts  = runFn0 newGraphics
  let btns = runFn0 newGraphics
  _    <- drawPopulation pop city (businessFractions city) (Color 0x8888FF) 0.0
  _    <- drawPopulation pop city (residentFractions city) (Color 0x88FF88) Math.pi
  _    <- foldl (drawRoadRoutes rts city) (return unit) (M.toList rm.roadRouteIds)
  _    <- drawRoads rds city
  _    <- foldl (drawPerimeter btns city) (return unit) (M.toList rm.perimeterRouteIds)
  _    <- foldl (drawButton btns rm) (return unit) (M.toList (stopsCoords city))
  _    <- runFn2 addToContainer pop  gfx
  _    <- runFn2 addToContainer rds  gfx
  _    <- runFn2 addToContainer rts  gfx
  _    <- runFn2 addToContainer btns gfx
  pure unit

buttonRadius = 15.0
extraPerimeterRadius = 2.0
maxExtraPopulationRadius = 15.0

drawButton btns rm acc (Tuple stopId stopCoords) = let
  inside = withGraphics [ beginFill (Color 0x4679BD) opaque 
                          , lineStyle (Width 2.0) (Color 0x4679BD) opaque 
                          , drawCircle stopCoords buttonRadius 
                          , endFill 
                          ]
  outsideIfSelected = if S.member stopId rm.selected
    then withGraphics [ lineStyle (Width 5.0) (Color 0xcfdc00) opaque 
                      , drawCircle stopCoords (buttonRadius + 5.0) 
                      ]
    else const $ return unit
  in acc *> do
    _ <- inside btns
    _ <- outsideIfSelected btns
    pure unit

drawRoads gfx city = let
  drawRoad (Pair s1 s2) = withCoords2 city drw s1 s2 where
    drw c1 c2 = withGraphics [ lineStyle (Width 3.0) (Color 0x111111) opaque 
                             , moveTo c1
                             , lineTo c2
                             ] gfx
  in foldl (\acc r -> acc *> (drawRoad r)) (return unit) (roads city)    

createRoadRoutesRect routes = let
  routeCount  = S.size routes
  routeHeight = Math.min 5.0 (buttonRadius*2.0/(toNumber routeCount))
  startingY   = negate (toNumber routeCount)/2.0*routeHeight
  drawRoute (Tuple r y) = let c = color r in
    withGraphics [ beginFill c opaque 
                 , lineStyle (Width 0.0) c opaque 
                 , drawRect { x: 0.0, y: y } 10.0 routeHeight 
                 , endFill 
                 ]
  routesWithY :: List (Tuple RouteId Number)
  routesWithY = let
    addRouteWithY r (Tuple result y) = Tuple ((Tuple r y) : result) (y + routeHeight) 
    in fst $ foldr addRouteWithY (Tuple Nil startingY) (S.toList routes)
  drawRoutes g = foldl (\acc t -> acc *> (drawRoute t g)) (return unit) routesWithY
  g = runFn0 newGraphics
  in do
    _ <- drawRoutes g
    pure g

drawRoadRoutes gfx city acc (Tuple (Pair s1 s2) routes) =
  acc *> withCoords2 city drw s1 s2 where
  drw c1 c2 = do
    rr <- createRoadRoutesRect routes
    _ <- runFn2 setWidth (distance c1 c2) rr
    let angle = Math.atan2 (c2.y-c1.y) (c2.x-c1.x)
    _ <- runFn2 setRotation angle rr
    _ <- runFn2 setPosition c1 rr
    _ <- runFn2 addToContainer rr gfx
    pure unit

drawPerimeter gfx city acc (Tuple s routes) =
  acc *> withCoords1 city drawAll s where
  routeCount = S.size routes
  routeArc = 2.0 * Math.pi / (toNumber routeCount)
  drawSingle c (Tuple r idx) = let
    startArc = routeArc*(toNumber idx)
    endArc = routeArc*(toNumber $ idx+1)
    in withGraphics [ moveTo c 
                    , lineStyle (Width 2.0) (color r) opaque 
                    , arc c (buttonRadius+extraPerimeterRadius) startArc endArc 
                    ] gfx
  routesWithIndex = zip (S.toList routes) (0 .. (routeCount - 1))
  drawAll c = foldl (\a t -> a *> (drawSingle c t)) (return unit) routesWithIndex

scaledFractions fracts = let
  max = fromMaybe 1.0 $ maximum $ M.values fracts
  in (\f -> f / max) <$> fracts

drawPopulation gfx city fracts color startArc = let
  scaled = scaledFractions fracts
  drawCoordPop acc fract coords = let
    radius = buttonRadius+extraPerimeterRadius+maxExtraPopulationRadius*fract
    in acc *> withGraphics [ beginFill color opaque 
                           , moveTo coords 
                           , lineStyle (Width 0.0) color opaque 
                           , arc coords radius startArc (startArc+Math.pi) 
                           , endFill 
                           ] gfx
  drawStopPop acc (Tuple s fract) = withCoords1 city (drawCoordPop acc fract) s
  in foldl drawStopPop (pure unit) (M.toList scaled)

withCoords1 city f s = fromMaybe (return unit) $ do
  c <- M.lookup s (stopsCoords city)
  pure $ f c

withCoords2 city f s1 s2 = fromMaybe (return unit) $ do
  c1 <- M.lookup s1 (stopsCoords city)
  c2 <- M.lookup s2 (stopsCoords city)
  pure $ f c1 c2
