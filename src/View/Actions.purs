module View.Actions where

import Signal.Time (Time)
import StopRoute (StopId)

data Action = NoOp | AnimationFrame Time | Click StopId
