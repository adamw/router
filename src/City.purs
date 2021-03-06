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
  , showStopWithPop
  ) where

import Prelude
import Route (StopId, RouteFragment)
import Data.Maybe (fromMaybe, Maybe(Just, Nothing))
import Data.Coords (Coords, distance)
import Data.Graph as G
import Data.Map as M
import Data.Foldable (foldl, sum)
import Data.Pair (Pair(Pair))
import Data.Set (Set, empty, fromFoldable, union) as S
import Data.Int (toNumber)
import Data.NonEmpty ((:|), NonEmpty)
import Data.Array as A

type PopulationMap = M.Map StopId Int

type CityData = 
  { width :: Number
  , height :: Number
  , stopsCoords :: M.Map StopId Coords
  , stopsGraph :: G.Graph StopId Number
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
  stopsGraph' = G.insertVertex stopId c.stopsGraph

addRoad :: StopId -> StopId -> City -> City
addRoad sid1 sid2 city@(City c) = fromMaybe city $ do
  c1 <- M.lookup sid1 c.stopsCoords
  c2 <- M.lookup sid2 c.stopsCoords
  let d = distance c1 c2
  let stopsGraph' = G.insertEdge sid1 sid2 d c.stopsGraph
  let stopsGraph'' = G.insertEdge sid2 sid1 d stopsGraph'
  pure $ City $ c { stopsGraph = stopsGraph'' }

stopsCoords :: City -> M.Map StopId Coords
stopsCoords (City c) = c.stopsCoords

routeFragment :: StopId -> StopId -> City -> RouteFragment
routeFragment s1 s2 (City c) =
  -- there should always be a path, as we assume the graph is connected
  toNonEmpty s1 (fromMaybe [] $ A.fromFoldable <$> G.shortestPath s1 s2 c.stopsGraph)

toNonEmpty :: forall a. a -> Array a -> NonEmpty Array a
toNonEmpty def a = case A.uncons a of
  Just { head: h, tail: t } -> h :| t
  Nothing -> def :| []

roads :: City -> S.Set (Pair StopId)
roads (City c) = let
  addForV acc v = S.union acc (S.fromFoldable edgesFromV) where
    edgesFromV = Pair v <$> G.adjacent v c.stopsGraph
  in foldl addForV S.empty (G.vertices c.stopsGraph)

lookupOr0 :: forall k. (Ord k) => k -> M.Map k Int -> Int
lookupOr0 k m = fromMaybe 0 $ M.lookup k m

setResidents :: StopId -> Int -> City -> City
setResidents s p (City c) = City $ c { residentCount = M.insert s p c.residentCount }

residents :: StopId -> City -> Int
residents s (City c) = lookupOr0 s c.residentCount

setBusinesses :: StopId -> Int -> City -> City
setBusinesses s p (City c) = City $ c { businessCount = M.insert s p c.businessCount }

businesses :: StopId -> City -> Int
businesses s (City c) = lookupOr0 s c.businessCount

total :: forall k v. (Semiring v) => M.Map k v -> v
total m = sum $ M.values m

fractions :: forall k. M.Map k Int -> M.Map k Number
fractions m = let
  t = toNumber $ total m
  in map (\p -> (toNumber p) / t) m

residentFractions :: City -> M.Map StopId Number
residentFractions (City { residentCount }) = fractions residentCount

businessFractions :: City -> M.Map StopId Number
businessFractions (City { businessCount }) = fractions businessCount

showStopWithPop :: StopId -> City -> String
showStopWithPop s c =
  (show s)
  <> ", residents: "
  <> (show $ residents s c)
  <> ", businesses: "
  <> (show $ businesses s c)
