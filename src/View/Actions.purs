module View.Actions where

import Data.Maybe (Maybe)
import Route (StopId, RouteId)
import Signal.Time (Time)

data Action = NoOp
            | AnimationFrame Time
            | Click StopId
            | Hover (Maybe StopId)
            | Complete RouteId
