module City
  ( City()
  , Coords
  , empty
  , addStop
  , addRoad
  ) where

import Prelude

import StopRoute

import Data.Set as S
import Data.Map as M
import Data.Tuple
import Data.Array
import Data.Maybe
import Data.ALGraph as G

import Data.Monoid.Additive

type Coords =
  { x :: Number
  , y :: Number
  }

type CityData = 
  { width :: Number
  , height :: Number
  , stopsCoords :: M.Map StopId Coords
  , stopsGraph :: G.ALGraph StopId (Additive Number)
  }

newtype City = City CityData

empty :: Number -> Number -> City
empty w h = City { width: w, height: h, stopsCoords: M.empty, stopsGraph: G.empty }

addStop :: Number -> Number -> City -> City
addStop x y (City c) =
  City $ c { stopsCoords = stopsCoords', stopsGraph = stopsGraph' } where
  stopId = newStopId ("stop" ++ (show $ M.size c.stopsCoords))
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
    
{-

Build routes:
* propose route - shortest path between two stops, using distance cache
* ? add proposed route
* 

Display:

-}
