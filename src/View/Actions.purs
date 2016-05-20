module View.Actions where

import Data.Maybe (Maybe)
import Route (StopId)
import Signal.Time (Time)

data Action = NoOp | AnimationFrame Time | Click StopId | Hover (Maybe StopId)
