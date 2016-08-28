module City
  ( City()
  , empty
  , width
  , height
  , addStop
  , addRoad
  , stopsCoords
  , routeFragment
  , roads
  , setResidents
  , residents
  , setBusinesses
  , businesses
  , residentFractions
  , businessFractions
  ) where

import Prelude
import Route
import Data.Maybe
import Data.Coords
import Data.Monoid.Additive
import Data.ALGraph as G
import Data.Map as M
import Data.Foldable (foldl, sum)
import Data.Pair (Pair(Pair))
import Data.Set (Set, insert, empty) as S
import Data.Tuple (snd)
import Data.Int (toNumber)

type PopulationMap = M.Map StopId Int

type CityData = 
  { width :: Number
  , height :: Number
  , stopsCoords :: M.Map StopId Coords
  , stopsGraph :: G.ALGraph StopId (Additive Number)
  , residentCount :: PopulationMap
  , businessCount :: PopulationMap
  }

newtype City = City CityData

empty :: Number -> Number -> City
empty w h = City $ { width: w
                   , height: h
                   , stopsCoords: M.empty
                   , stopsGraph: G.empty
                   , residentCount: M.empty
                   , businessCount: M.empty }

width :: City -> Number
width (City c) = c.width

height :: City -> Number
height (City c) = c.height

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
  pure $ City $ c { stopsGraph = stopsGraph' }

stopsCoords :: City -> M.Map StopId Coords
stopsCoords (City c) = c.stopsCoords

routeFragment :: StopId -> StopId -> City -> RouteFragment
routeFragment s1 s2 (City c) =
  G.shortestPath s1 s2 c.stopsGraph

roads :: City -> S.Set (Pair StopId)
roads (City c) = let
  addForV acc v = foldl (\a e -> S.insert (Pair v (snd e)) a) acc (G.edgesFrom v c.stopsGraph)
  in foldl addForV S.empty (G.vertices c.stopsGraph)

lookupOr0 k m = fromMaybe 0 $ M.lookup k m

setResidents :: StopId -> Int -> City -> City
setResidents s p (City c) = City $ c { residentCount = M.insert s p c.residentCount }

residents :: StopId -> City -> Int
residents s (City c) = lookupOr0 s c.residentCount

setBusinesses :: StopId -> Int -> City -> City
setBusinesses s p (City c) = City $ c { businessCount = M.insert s p c.businessCount }

businesses :: StopId -> City -> Int
businesses s (City c) = lookupOr0 s c.businessCount

total m = sum $ M.values m

fractions m = let
  t = toNumber $ total m
  in map (\p -> (toNumber p) / t) m

residentFractions :: City -> M.Map StopId Number
residentFractions (City { residentCount }) = fractions residentCount

businessFractions :: City -> M.Map StopId Number
businessFractions (City { businessCount }) = fractions businessCount
