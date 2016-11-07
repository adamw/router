module Assignment
  ( Assignment
  , empty
  , update
  , addBus
  , removeBus
  , selectStop
  , tooltip
  ) where

import Prelude
import Data.List as L
import Data.Map as M
import Data.Set as S
import City (showStopWithPop, City)
import Data.Foldable (fold)
import Data.Maybe (maybe, Maybe(Just, Nothing))
import Data.Monoid.Additive (runAdditive, Additive(Additive))
import Data.Tuple (fst)
import Data.Sequence as SQ
import Route (StopId, Routes, RouteId)

type Assignment =
  { city :: City
  , total :: Int
  , available :: Int
  , routes :: Routes
  , buses :: M.Map RouteId Int
  , selectedStop :: Maybe StopId
  }

empty :: Int -> City -> Assignment
empty a c = { city: c
            , total: a
            , available: a
            , routes: SQ.empty
            , buses: M.empty
            , selectedStop: Nothing
            }

update :: Routes -> Assignment -> Assignment
update rs a = let
  ids = S.fromFoldable $ _.routeId <$> rs
  buses' = M.toList a.buses # L.filter (\p -> S.member (fst p) ids) # M.fromList
  used = M.values buses' # (map Additive) # fold # runAdditive
  in a { routes    =  rs
       , available = a.total - used
       , buses     = buses'
       }

addBus :: RouteId -> Assignment -> Assignment
addBus rid a = if a.available > 0
  then a { available = a.available - 1
         , buses  = M.alter (Just <<< maybe 1 (_ + 1)) rid a.buses
         }
  else a

removeBus :: RouteId -> Assignment -> Assignment
removeBus rid a = case M.lookup rid a.buses of
  Just c -> a { available = a.available + 1
              , buses = M.insert rid (c-1) a.buses
              }
  Nothing -> a

selectStop :: Maybe StopId -> Assignment -> Assignment
selectStop s a = a { selectedStop = s }
     
tooltip :: Assignment -> Maybe String
tooltip a = (\s -> showStopWithPop s a.city) <$> a.selectedStop
