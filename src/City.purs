module City
  ( City()
  , empty
  , addStop
  , addRoad
  , stopsCoords
  , routeFragment
  ) where

import Prelude

import Route

import Data.Set as S
import Data.Map as M
import Data.Tuple
import Data.Array
import Data.Maybe
import Data.ALGraph as G
import Data.Coords

import Data.Monoid.Additive

type CityData = 
  { width :: Number
  , height :: Number
  , stopsCoords :: M.Map StopId Coords
  , stopsGraph :: G.ALGraph StopId (Additive Number)
  }

newtype City = City CityData

empty :: Number -> Number -> City
empty w h = City { width: w, height: h, stopsCoords: M.empty, stopsGraph: G.empty }

addStop :: Number -> Number -> StopId -> City -> City
addStop x y stopId (City c) =
  City $ c { stopsCoords = stopsCoords', stopsGraph = stopsGraph' } where
  stopsCoords' = M.insert stopId { x: x, y : y} c.stopsCoords
  stopsGraph' = G.addV stopId c.stopsGraph

addRoad :: StopId -> StopId -> City -> City
addRoad sid1 sid2 city@(City c) = fromMaybe city $ do
  c1 <- M.lookup sid1 c.stopsCoords
  c2 <- M.lookup sid2 c.stopsCoords
  let d = distance c1 c2
  let stopsGraph' = G.addE sid1 sid2 (Additive d) c.stopsGraph
  return $ City $ c { stopsGraph = stopsGraph' }

distance :: Coords -> Coords -> Number
distance c1 c2 = Math.sqrt $ (Math.pow (c1.x-c2.x) 2.0) + (Math.pow (c1.y-c2.y) 2.0)

stopsCoords :: City -> M.Map StopId Coords
stopsCoords (City c) = c.stopsCoords

routeFragment :: StopId -> StopId -> City -> RouteFragment
routeFragment s1 s2 (City c) =
  G.shortestPath s1 s2 c.stopsGraph

