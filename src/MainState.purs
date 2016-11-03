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
import EditorMain as EditorMain
import View.Fps as FpsView
import View.Messages as MsgsView
import View.Modal as Modal
import Assignment (Assignment)
import Data.Either (Either(Left, Right))
import Data.Int (floor)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Modes (Mode(EditorMode, AssignmentMode))
import Route (StopId)
import View.Modal (dimap)
import Control.Alt ((<|>))

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

updated :: State -> Boolean
updated (State state) = state.updated

mode :: State -> Mode
mode (State state) = state.mode

editor :: State -> Editor
editor (State state) = state.editor

assignment :: State -> Assignment
assignment (State state) = state.assignment

modal :: State -> Maybe (Modal.ModalState State)
modal (State state) = state.modal

fps :: State -> FpsView.FpsState
fps (State state) = state.fps

msgs :: State -> MsgsView.MsgsState
msgs (State state) = state.msgs

-- modifiers

setNotUpdated :: StateTr
setNotUpdated (State s) = State $ s { updated = false }

setUpdated :: StateTr
setUpdated (State s) = State $ s { updated = true }

setMsg :: String -> StateTr
setMsg m (State state) = State $ state { msgs = MsgsView.update m state.msgs }

setMode :: Mode -> StateTr
setMode m (State state) = State $ state { mode = m }

setEditor :: Editor -> StateTr
setEditor e (State state) = State $ state { editor = e }

setAssignment :: Assignment -> StateTr
setAssignment a (State state) = State $ state { assignment = a }

setModal :: Maybe (Modal.ModalState State) -> StateTr
setModal m (State state) = State $ state { modal = m }

setActionTooltip :: Maybe String -> StateTr
setActionTooltip t (State state) = State $ state { tooltip = state.tooltip { fromAction = t } }

setStateTooltip :: (State -> Maybe String) -> StateTr
setStateTooltip f (State state) = State $ state { tooltip = state.tooltip { fromState = f } }

setOnStop :: (Maybe StopId -> StateTr) -> (StopId -> StateTr) -> StateTr
setOnStop hover click (State state) = State $ state { onStopHover = hover
                                                    , onStopClick = click }

-- helper

identity :: forall t. t -> t
identity x = x
