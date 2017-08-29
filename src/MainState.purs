module MainState
  ( State
  , empty
  , step
  , tooltip
  , updated
  , mode
  , editor
  , assignment
  , modal
  , fps
  , msgs
  )where

import Prelude
import Editor
import View.Actions
import Assignment as Assignment
import AssignmentMain as AssignmentMain
import EditorMain as EditorMain
import View.Fps as FpsView
import View.Messages as MsgsView
import View.Modal as Modal
import Assignment (Assignment)
import Control.Alt ((<|>))
import Data.Either (Either(Left, Right))
import Data.Int (floor)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Tuple (Tuple(Tuple))
import Modes (Mode(EditorMode, AssignmentMode))
import Route (StopId)
import View.Modal (dimap)

type Tooltip =
  { fromAction :: Maybe String
  , fromState :: State -> Maybe String
  }

type StateTr = State -> State

newtype State = State
  { fps :: FpsView.FpsState
  , msgs :: MsgsView.MsgsState
  , tooltip :: Tooltip
  , mode :: Mode
  , editor :: Editor
  , assignment :: Assignment.Assignment
  , modal :: Maybe (Modal.ModalState State)
  , updated :: Boolean
  , onStopHover :: Maybe StopId -> StateTr
  , onStopClick :: StopId -> StateTr
  }

empty :: Assignment -> FpsView.FpsState -> MsgsView.MsgsState -> Editor -> State
empty _assignment _fps _msgs _editor = let
  state = State $ { fps: _fps
                  , msgs: _msgs
                  , tooltip: { fromAction: Nothing, fromState: \_ -> Nothing }
                  , mode: EditorMode
                  , editor: _editor
                  , assignment: _assignment
                  , modal: Nothing
                  , updated: true
                  , onStopHover: \_ -> identity
                  , onStopClick: \_ -> identity
                  }
  in adjustStateToMode EditorMode state

step :: Action -> StateTr
step (AnimationFrame nowMillis) (State state) = setNotUpdated $ State $ state
  { fps = FpsView.update (floor (nowMillis / 1000.0)) state.fps }
step (RouteMapAction (Click stopId)) s@(State state) =
  (setUpdated <<< setMsg ("You clicked " <> (show stopId)) <<< state.onStopClick stopId) s
step (RouteMapAction (Hover stopId)) s@(State state) =
  (setUpdated <<< setMsg ("Hovering " <> (show stopId)) <<< state.onStopHover stopId) s
step (EditorAction ea) s@(State state) =
  case EditorMain.step ea state.editor of
    Right editor' -> (setUpdated <<< setEditor editor') s
    Left  _modal  -> let
      prjEditor (State state) = state.editor
      modal' = dimap prjEditor setEditor _modal
      in (setUpdated <<< setModal (Just modal')) s
step (AssignmentAction aa) s@(State state) = (setUpdated <<< setAssignment assignment') s where
  assignment' = AssignmentMain.step aa state.assignment
step (ModalAction modalAction) s@(State state) =
  case Modal.update state.modal modalAction s of
    Tuple s' modal' -> (setUpdated <<< setMsg "Modal" <<< setModal modal') s'
step (TooltipAction (ShowTooltip tt)) s =
  (setNotUpdated <<< setMsg "Show tooltip" <<< setActionTooltip tt) s
step (TooltipAction ClearTooltip) s =
  (setNotUpdated <<< setMsg "Clear tooltip" <<< setActionTooltip Nothing) s
step (SwitchToMode m) s = (setUpdated <<< setMode m <<< adjustStateToMode m) s
step NoOp s = s

adjustStateToMode :: Mode -> StateTr
adjustStateToMode EditorMode s =
  (setStateTooltip stt <<< setOnStop osh osc) s where
  stt (State state) = Just $ editorTooltip state.editor
  osh stopId s@(State state) = setEditor (candidateStop stopId state.editor) s
  osc stopId s@(State state) = setEditor (selectStop stopId state.editor) s
adjustStateToMode AssignmentMode s@(State state) =
  (setStateTooltip stt <<< setAssignment a' <<< setOnStop osh (\_ -> identity)) s where
  stt (State state) = Assignment.tooltip state.assignment   
  a' = Assignment.update state.editor.routes state.assignment
  osh stopId s@(State state) = setAssignment (Assignment.selectStop stopId state.assignment) s
adjustStateToMode _ s = s

tooltip :: State -> Maybe String
tooltip ss@(State state) = state.tooltip.fromAction <|> (state.tooltip.fromState ss)

-- accessors

derive instance newtypeState :: Newtype State _

get :: forall t a b. (Newtype t a) => (a -> b) -> t -> b
get f t = f (unwrap t)

updated :: State -> Boolean
updated = get _.updated

mode :: State -> Mode
mode = get _.mode

editor :: State -> Editor
editor = get _.editor

assignment :: State -> Assignment
assignment = get _.assignment

modal :: State -> Maybe (Modal.ModalState State)
modal = get _.modal

fps :: State -> FpsView.FpsState
fps = get _.fps

msgs :: State -> MsgsView.MsgsState
msgs = get _.msgs

-- modifiers

set :: forall t a. (Newtype t a) => (a -> a) -> t -> t
set f t = wrap (f (unwrap t))

setNotUpdated :: StateTr
setNotUpdated = set (_ {updated = false})

setUpdated :: StateTr
setUpdated = set (_ { updated = true })

setMsg :: String -> StateTr
setMsg m s@(State state) = set (_ { msgs = MsgsView.update m state.msgs }) s

setMode :: Mode -> StateTr
setMode m = set (_ { mode = m })

setEditor :: Editor -> StateTr
setEditor e = set (_ { editor = e })

setAssignment :: Assignment -> StateTr
setAssignment a = set (_ { assignment = a })

setModal :: Maybe (Modal.ModalState State) -> StateTr
setModal m = set (_ { modal = m })

setActionTooltip :: Maybe String -> StateTr
setActionTooltip t = set (_ { tooltip { fromAction = t } })

setStateTooltip :: (State -> Maybe String) -> StateTr
setStateTooltip f = set (_ { tooltip { fromState = f } })

setOnStop :: (Maybe StopId -> StateTr) -> (StopId -> StateTr) -> StateTr
setOnStop hover click = set (_ { onStopHover = hover
                               , onStopClick = click })

-- helper

identity :: forall t. t -> t
identity x = x
