module Assignment where

import Prelude
import Data.Map as M
import City (City)
import Route (Routes, RouteId)

type Assignment =
  { available :: Int
  , buses :: M.Map RouteId Int
  }

emptyAssignment :: City -> Assignment
emptyAssignment c = { available: 0, buses: M.empty }

update :: Routes -> Assignment -> Assignment
update rs a = a
