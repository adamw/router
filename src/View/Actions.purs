module View.Actions where

import Signal.Time (Time)
import Route (StopId)

data Action = NoOp | AnimationFrame Time | Click StopId
