module View.RoutesMap
  ( draw
  , setupButtons
  ) where

import Prelude
import Pixi
import Data.Map as M
import Data.Set as S
import Math as Math
import ChSend (ChSend)
import City (residentFractions, businessFractions, stopsCoords, roads, City)
import Data.Coords (origin2D, distance)
import Data.Foldable (maximum, foldr, foldl)
import Data.Function.Uncurried (runFn2, runFn0)
import Data.Int (toNumber)
import Data.List (List(Nil), (:), zip, (..))
import Data.Maybe (Maybe(Nothing, Just), fromMaybe)
import Data.Pair (Pair(Pair))
import Data.Tuple (fst, Tuple(Tuple))
import Route (RouteId, StopId)
import RoutesMap (RoutesMap)
import View.Actions (RouteMapAction(Hover, Click))
import View.Route (color)

-- constants

buttonRadius = 15.0
extraPerimeterRadius = 2.0
maxExtraPopulationRadius = 15.0

-- buttons

setupButtons :: forall t. ChSend RouteMapAction -> City -> PixiChEff t Container
setupButtons ch city = let btns = runFn0 newContainer in do
  _    <- foldl (setupButton ch btns) (pure unit) (toList (stopsCoords city))
  pure btns

setupButton ch btns acc (Tuple stopId stopCoords) = acc *> let g = runFn0 newGraphics in do
  ha <- runFn2 newCircle origin2D buttonRadius
  _  <-        newButton ha g
  _  <-        addToContainerAt g stopCoords  btns
  _  <-        onMouseDown ch (Click stopId) g
  _  <-        onMouseHover ch (Hover (Just stopId)) (Hover Nothing) g
  pure unit

-- map drawing

draw :: forall t. Graphics -> City -> RoutesMap -> PixiEff t Unit
draw gfx city rm = do
  _    <- removeAllFromContainer gfx
  let pop   = runFn0 newGraphics
  let rds   = runFn0 newGraphics
  let rts   = runFn0 newGraphics
  let stops = runFn0 newGraphics
  _    <- drawPopulation pop city (businessFractions city) (Color 0x8888FF) 0.0
  _    <- drawPopulation pop city (residentFractions city) (Color 0x88FF88) Math.pi
  _    <- foldl (drawRoadRoutes rts city) (pure unit) (toList rm.roadRouteIds)
  _    <- drawRoads rds city
  _    <- foldl (drawPerimeter stops city) (pure unit) (toList rm.perimeterRouteIds)
  _    <- foldl (drawStop stops rm) (pure unit) (toList (stopsCoords city))
  _    <- runFn2 addToContainer pop   gfx
  _    <- runFn2 addToContainer rds   gfx
  _    <- runFn2 addToContainer rts   gfx
  _    <- runFn2 addToContainer stops gfx
  pure unit

drawStop stops rm acc (Tuple stopId stopCoords) = let
  inside = withGraphics [ beginFill (Color 0x4679BD) opaque 
                        , lineStyle (Width 2.0) (Color 0x4679BD) opaque 
                        , drawCircle stopCoords buttonRadius 
                        , endFill 
                        ]
  outsideIfSelected = if S.member stopId rm.selected
    then withGraphics [ lineStyle (Width 5.0) (Color 0xcfdc00) opaque 
                      , drawCircle stopCoords (buttonRadius + 5.0) 
                      ]
    else const $ pure unit
  in acc *> do
    _ <- inside stops
    _ <- outsideIfSelected stops
    pure unit

drawRoads gfx city = let
  drawRoad (Pair s1 s2) = withCoords2 city drw s1 s2 where
    drw c1 c2 = withGraphics [ lineStyle (Width 3.0) (Color 0x111111) opaque 
                             , moveTo c1
                             , lineTo c2
                             ] gfx
  in foldl (\acc r -> acc *> (drawRoad r)) (pure unit) (roads city)

createRoadRoutesRect :: forall r. S.Set RouteId -> PixiEff r Graphics 
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
    in fst $ foldr addRouteWithY (Tuple Nil startingY) (S.toUnfoldable routes :: List RouteId)
  drawRoutes g = foldl (\acc t -> acc *> (drawRoute t g)) (pure unit) routesWithY
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
  routesWithIndex = zip (S.toUnfoldable routes) (0 .. (routeCount - 1))
  drawAll c = foldl (\a t -> a *> (drawSingle c t)) (pure unit) routesWithIndex

scaledFractions fracts = let
  max = fromMaybe 1.0 $ maximum $ M.values fracts
  in (\f -> f / max) <$> fracts

drawPopulation :: forall r. Graphics -> City -> M.Map StopId Number -> Color -> Number -> PixiEff r Unit
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
  in foldl drawStopPop (pure unit) (toList scaled)

withCoords1 city f s = fromMaybe (pure unit) $ do
  c <- M.lookup s (stopsCoords city)
  pure $ f c

withCoords2 city f s1 s2 = fromMaybe (pure unit) $ do
  c1 <- M.lookup s1 (stopsCoords city)
  c2 <- M.lookup s2 (stopsCoords city)
  pure $ f c1 c2

toList :: forall k v. M.Map k v -> List (Tuple k v)
toList = M.toUnfoldable
