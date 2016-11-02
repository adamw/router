module Main where

import Prelude
import Signal
import Editor
import Pixi
import View.Actions
import View.Dimensions
import ChSend
import Assignment as Assignment
import City as City
import EditorMain as EditorMain
import Signal.Channel as SignalCh
import Signal.DOM as SignalDOM
import TheCity as TheCity
import View.Assignment as AssignmentView
import View.Fps as FpsView
import View.Messages as MsgsView
import View.Modal as Modal
import View.ModePicker as ModePicker
import View.Tooltip as TooltipView
import Assignment (Assignment)
import Control.Alt ((<|>))
import DOM.HTML.Location (assign)
import Data.Coords (origin2D)
import Data.Either (Either(Left, Right))
import Data.Function.Uncurried (runFn0, runFn2)
import Data.Int (floor)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Modes (Mode(EditorMode, SimulationMode, AssignmentMode))
import Route (StopId)
import View.Modal (dimap)

type Tooltip =
  { fromAction :: Maybe String
  , fromState :: State -> Maybe String
  }

identity x = x

-- TODO: extract state to a separate file
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

newtype ViewState = ViewState
  { renderer :: Renderer
  , actionCh :: ChSend Action
  , stage :: Container
  , fps :: FpsView.FpsViewState
  , msgs :: MsgsView.MsgsViewState
  , tooltip :: TooltipView.TooltipViewState
  , mode :: Mode
  , editor :: EditorMain.ViewState
  , assignment :: AssignmentView.ViewState
  , modePicker :: ModePicker.ViewState
  , nextEffect :: AnyEff
  , modal :: Maybe Modal.ModalViewState
  }

main = do
  actionCh     <- SignalCh.channel NoOp
  Tuple initState initViewState <- setup (chSend actionCh)
  animationSig <- SignalDOM.animationFrame
  let mainSig   = merge (SignalCh.subscribe actionCh) (AnimationFrame <$> animationSig)
  let stepSig   = foldp step initState mainSig
  let viewStepSig = foldp draw initViewState stepSig
  let nextEff :: ViewState -> forall r. PixiChEff r Unit
      nextEff (ViewState { nextEffect: AnyEff nextEffect }) = nextEffect
  let effectSig = nextEff <$> viewStepSig
  runSignal effectSig

pickerH = 1.5*boxH
modeViewCoords = { x: 0.0, y: pickerH }

setup :: forall r. (ChSend Action) -> PixiChEff r (Tuple State ViewState)
setup ch = let
  city     = TheCity.city
  cityW    = City.width city
  totalW   = cityW + sideBarW
  tooltipH = boxH
  totalH   = pickerH + City.height city + tooltipH
  r        = runFn2 newRenderer (floor totalW) (floor totalH)
  assignment = Assignment.empty TheCity.buses city
  in do
    _    <- runFn2 setBgColor 0x999999 r
    _    <- appendRendererToBody r
    let s = runFn0 newContainer
    Tuple fps fpsView  <- FpsView.setup s
    Tuple msgs msgsView <- MsgsView.setup s
    Tuple editor editorViewState <- EditorMain.setup ch city totalH
    _    <- addToContainerAt (EditorMain.container editorViewState) modeViewCoords s
    tooltipView <- TooltipView.setup totalW tooltipH
    _    <- addToContainerAt tooltipView.gfx { x: 0.0, y: totalH - tooltipH } s
    assignmentViewState <- AssignmentView.setup ch city
    let modePicker = ModePicker.setup totalW pickerH
    _    <- addToContainerAt modePicker.gfx origin2D s
    let initState = { fps: fps
                    , msgs: msgs
                    , tooltip: { fromAction: Nothing, fromState: \_ -> Nothing }
                    , mode: EditorMode
                    , editor: editor
                    , assignment: assignment
                    , modal: Nothing
                    , updated: true
                    , onStopHover: \_ -> identity
                    , onStopClick: \_ -> identity
                    }
    let initViewState = ViewState { renderer: r
                        , actionCh: ch
                        , stage: s
                        , fps: fpsView
                        , msgs: msgsView
                        , tooltip: tooltipView
                        , mode: initState.mode
                        , editor: editorViewState
                        , assignment: assignmentViewState
                        , modePicker: modePicker
                        , nextEffect: AnyEff (pure unit)
                        , modal: Nothing
                        }
    pure (Tuple (adjustModeState EditorMode (State initState)) initViewState)

step :: Action -> StateTr
step (AnimationFrame nowMillis) (State state) = State $ state
  { fps     = FpsView.update (floor (nowMillis / 1000.0)) state.fps
  , updated = false
  }
step (RouteMapAction (Click stopId)) s@(State state) =
  (updated <<< setMsg ("You clicked " <> (show stopId)) <<< state.onStopClick stopId) s
step (RouteMapAction (Hover stopId)) s@(State state) =
  (updated <<< setMsg ("Hovering " <> (show stopId)) <<< state.onStopHover stopId) s
step (EditorAction ea) (State state) =
  case EditorMain.step ea state.editor of
    Right editor' -> updated $ State $ state { editor = editor' }
    Left  modal   -> let
      prjEditor (State state) = state.editor
      injEditor e (State state) = updated $ State $ state { editor = e }
      modal' = dimap prjEditor injEditor modal
      in updated $ State $ state { modal = Just modal' }
step (ModalAction modalAction) (State state) =
  case Modal.update state.modal modalAction (State state) of
    Tuple (State state') modal' -> State $ state'
                                     { msgs    = MsgsView.update ("Modal") state.msgs
                                     , updated = true                  
                                     , modal   = modal'
                                     }
step (TooltipAction (ShowTooltip tooltip)) s =
  (notUpdated <<< setMsg "Show tooltip" <<< setActionTooltip tooltip) s
step (TooltipAction ClearTooltip) s =
  (notUpdated <<< setMsg "Clear tooltip" <<< setActionTooltip Nothing) s
step (SwitchToMode m) s = (updated <<< setMode m <<< adjustModeState m) s
step NoOp s = s

notUpdated :: StateTr
notUpdated (State s) = State $ s { updated = false }

updated :: StateTr
updated (State s) = State $ s { updated = true }

setMsg :: String -> StateTr
setMsg m (State state) = State $ state { msgs = MsgsView.update m state.msgs }

setMode :: Mode -> StateTr
setMode m (State state) = State $ state { mode = m }

setEditor :: Editor -> StateTr
setEditor e (State state) = State $ state { editor = e }

setAssignment :: Assignment -> StateTr
setAssignment a (State state) = State $ state { assignment = a }

setActionTooltip :: Maybe String -> StateTr
setActionTooltip t (State state) = State $ state { tooltip = state.tooltip { fromAction = t } }

setStateTooltip :: (State -> Maybe String) -> StateTr
setStateTooltip f (State state) = State $ state { tooltip = state.tooltip { fromState = f } }

setOnStopHover :: (Maybe StopId -> StateTr) -> StateTr
setOnStopHover f (State state) = State $ state { onStopHover = f }

setOnStopClick :: (StopId -> StateTr) -> StateTr
setOnStopClick f (State state) = State $ state { onStopClick = f }

adjustModeState :: Mode -> StateTr
adjustModeState EditorMode s =
  (setStateTooltip stt <<< setOnStopHover osh <<< setOnStopClick osc) s where
  stt (State state) = Just $ editorTooltip state.editor
  osh stopId s@(State state) = setEditor (candidateStop stopId state.editor) s
  osc stopId s@(State state) = setEditor (selectStop stopId state.editor) s
adjustModeState AssignmentMode s@(State state) =
  (setStateTooltip stt <<< setAssignment a' <<< setOnStopHover osh
   <<< setOnStopClick (\_ -> identity)) s where
    -- TODO: should be set together - illegal to set only one handler
  stt (State state) = Assignment.tooltip state.assignment   
  a' = Assignment.update state.editor.routes state.assignment
  osh stopId s@(State state) = setAssignment (Assignment.selectStop stopId state.assignment) s
adjustModeState _ s = s

draw :: State -> ViewState -> ViewState
draw ss@(State state) vss@(ViewState viewState) =
  case Modal.draw viewState.stage viewState.actionCh state.modal viewState.modal of
    Tuple modal' (AnyEff modalEff) -> let
      tooltip    = (state.tooltip.fromAction <|> (state.tooltip.fromState ss))
      nextEffect :: forall r. PixiChEff r Unit
      nextEffect = do
        _ <- adjustModeView ss vss
        _ <- FpsView.draw state.fps viewState.fps
        _ <- MsgsView.draw state.msgs viewState.msgs
        _ <- TooltipView.draw tooltip viewState.tooltip
        _ <- if state.updated      
             then do
               _ <- ModePicker.draw state.mode viewState.actionCh viewState.modePicker
               _ <- drawModeView state.mode ss vss
               pure unit
             else pure unit
        _ <- modalEff
        _ <- runFn2 renderContainer viewState.stage viewState.renderer
        pure unit
      in ViewState (viewState { modal      = modal'
                              , nextEffect = AnyEff nextEffect
                              , mode       = state.mode })

adjustModeView :: forall r. State -> ViewState -> PixiEff r Unit
adjustModeView (State s) vss@(ViewState vs) | s.mode /= vs.mode = do
  _ <- removeModeView vs.mode vss
  _ <- addModeView s.mode vss
  pure unit
adjustModeView _ vss = pure unit

addModeView :: forall r. Mode -> ViewState -> PixiEff r Unit
addModeView EditorMode (ViewState vs) =
  addToContainerAt (EditorMain.container vs.editor) modeViewCoords vs.stage
addModeView AssignmentMode (ViewState vs) =
  addToContainerAt (AssignmentView.container vs.assignment) modeViewCoords vs.stage
addModeView SimulationMode (ViewState vs) = pure unit

removeModeView :: forall r. Mode -> ViewState -> PixiEff r Unit
removeModeView EditorMode (ViewState vs)     =
  removeFromContainer (EditorMain.container vs.editor)
removeModeView AssignmentMode (ViewState vs) =
  removeFromContainer (AssignmentView.container vs.assignment)
removeModeView SimulationMode (ViewState vs) = pure unit

drawModeView :: forall r. Mode -> State -> ViewState -> PixiChEff r Unit
drawModeView EditorMode (State s) (ViewState vs) =
  EditorMain.draw vs.actionCh s.editor vs.editor
drawModeView AssignmentMode (State s) (ViewState vs) = AssignmentView.draw s.assignment vs.assignment
drawModeView SimulationMode _ _ = pure unit
