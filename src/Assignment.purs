module Assignment
  ( Assignment
  , empty
  , update
  ) where

import Prelude
import Data.List as L
import Data.Map as M
import Data.Set as S
import City (City)
import Data.Foldable (fold)
import Data.Monoid.Additive (runAdditive, Additive(Additive))
import Data.Tuple (fst)
import Route (Routes, RouteId)
import RoutesMap (create, empty, RoutesMap) as RoutesMap

type Assignment =
  { city :: City
  , total :: Int
  , available :: Int
  , buses :: M.Map RouteId Int
  , routesMap :: RoutesMap.RoutesMap
  }

empty :: Int -> City -> Assignment
empty a c = { city: c
            , total: a
            , available: a
            , buses: M.empty
            , routesMap: RoutesMap.empty
            }

update :: Routes -> Assignment -> Assignment
update rs a = let
  ids = S.fromFoldable $ _.routeId <$> rs
  buses' = M.toList a.buses # L.filter (\p -> S.member (fst p) ids) # M.fromList
  used = M.values buses' # (map Additive) # fold # runAdditive
  rm = RoutesMap.create rs S.empty
  in a { available = a.total - used
       , buses     = buses'
       , routesMap = rm
       }
                
