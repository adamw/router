module StopRoute
  ( Color
  , StopId()
  , newStopId
  , RouteFragment
  , Route
  ) where

import Prelude

type Color = String

newtype StopId = StopId String

newStopId :: String -> StopId
newStopId = StopId

instance eqStopId :: Eq StopId where
  eq (StopId sid1) (StopId sid2) = eq sid1 sid2

instance ordsStopId :: Ord StopId where
  compare (StopId sid1) (StopId sid2) = compare sid1 sid2

type RouteFragment = Array StopId -- consecutive stops

type Route =
  { color :: Color
  , fragments :: Array RouteFragment -- start of fragment n+1 should be == to end of fragment n
  }
