module View.Actions where

import Data.Maybe (Maybe)
import Modes (Mode)
import Route (RouteId, StopId)
import Signal.Time (Time)

data Action = NoOp
            | AnimationFrame Time
            | RouteMapAction RouteMapAction
            | EditorAction EditorAction
            | ModalAction ModalAction
            | TooltipAction TooltipAction
            | SwitchToMode Mode

data RouteMapAction = Click StopId | Hover (Maybe StopId)
data EditorAction = CompleteRoute
                  | RemoveLastStop
                  | RemoveRoute RouteId
                  | EditRoute RouteId
data TooltipAction = ShowTooltip (Maybe String) | ClearTooltip
data ModalAction = ModalOk | ModalCancel              
