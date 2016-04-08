module City
  ( Stop
  , City()
  ) where

import Prelude

import Data.Route

import Data.Set as S
import Data.Map as M
import Data.Tuple
import Data.Array
import Data.Maybe

type Stop =
  { id :: StopId
  , x :: Number
  , y :: Number
  , nghs :: S.Set StopId
  }

type CityData = 
  { width :: Number
  , height :: Number
  , stopsById :: M.Map StopId Stop
  , distances :: M.Map (Tuple StopId StopId) Number
  }

newtype City = City CityData

empty :: Number -> Number -> City
empty w h = City { width: w, height: h, stopsById: M.empty, distances: M.empty }

updateStopsById :: Stop -> CityData -> CityData
updateStopsById s c = c { stopsById = M.insert s.id s c.stopsById }

updateDistances :: StopId -> StopId -> Number -> CityData -> CityData
updateDistances sid1 sid2 d c = c { distances = M.insert (Tuple sid1 sid2) d c.distances }

addStop :: Number -> Number -> City -> City
addStop x y (City c) =
  City $ updateStopsById stop c where
  stopId = newStopId ("stop" ++ (show $ M.size c.stopsById))
  stop :: Stop
  stop = { id: stopId, x: x, y: y, nghs: S.empty }

addRoad :: StopId -> StopId -> City -> City
addRoad sid1 sid2 city@(City c) = fromMaybe city $ do
  s1 <- M.lookup sid1 c.stopsById
  s2 <- M.lookup sid2 c.stopsById
  let addNgh s n = s { nghs = S.insert n s.nghs }
  let s1' = addNgh s1 sid2
  let s2' = addNgh s2 sid1
  let d = distance s1 s2
  let us = updateStopsById s1' <<< updateStopsById s2'
  let ud = updateDistances sid1 sid2 d <<< updateDistances sid2 sid1 d
  return $ City $ us <<< ud $ c

distance :: Stop -> Stop -> Number
distance s1 s2 = Math.sqrt $ (Math.pow (s1.x-s2.x) 2.0) + (Math.pow (s1.y-s2.y) 2.0)
    
{-
Build city:
* create empty city
* add stop
* add path between stops

Build routes:
* propose route - shortest path between two stops, using distance cache
* ? add proposed route
* 

Display:

-}
