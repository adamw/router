module Main where

import Prelude
import Signal
import Pixi
import View.Actions
import View.Dimensions
import ChSend
import Assignment as Assignment
import City as City
import EditorMain as EditorMain
import MainState as State
import Signal.Channel as SignalCh
import Signal.DOM as SignalDOM
import TheCity as TheCity
import View.Assignment as AssignmentView
import View.Fps as FpsView
import View.Messages as MsgsView
import View.Modal as Modal
import View.ModePicker as ModePicker
import View.Tooltip as TooltipView
import Control.Alt ((<|>))
import Data.Coords (origin2D)
import Data.Function.Uncurried (runFn0, runFn2)
import Data.Int (floor)
import Data.Maybe (Maybe(Nothing))
import Data.Tuple (Tuple(Tuple))
import Modes (Mode(EditorMode, SimulationMode, AssignmentMode))

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
  let stepSig   = foldp State.step initState mainSig
  let viewStepSig = foldp draw initViewState stepSig
  let nextEff :: ViewState -> forall r. PixiChEff r Unit
      nextEff (ViewState { nextEffect: AnyEff nextEffect }) = nextEffect
  let effectSig = nextEff <$> viewStepSig
  runSignal effectSig

pickerH = 1.5*boxH
modeViewCoords = { x: 0.0, y: pickerH }

setup :: forall r. (ChSend Action) -> PixiChEff r (Tuple State.State ViewState)
setup ch = let
  city       = TheCity.city
  cityW      = City.width city
  totalW     = cityW + sideBarW
  tooltipH   = boxH
  totalH     = pickerH + City.height city + tooltipH
  r          = runFn2 newRenderer (floor totalW) (floor totalH)
  s          = runFn0 newContainer
  modePicker = ModePicker.setup totalW pickerH
  assignment = Assignment.empty TheCity.buses city
  in do
    Tuple fps fpsView <- FpsView.setup s
    Tuple msgs msgsView <- MsgsView.setup s
    Tuple editor editorViewState <- EditorMain.setup ch city totalH
    assignmentViewState <- AssignmentView.setup ch city
    tooltipView <- TooltipView.setup totalW tooltipH
    _    <- runFn2 setBgColor 0x999999 r
    _    <- appendRendererToBody r
    _    <- addToContainerAt tooltipView.gfx { x: 0.0, y: totalH - tooltipH } s
    _    <- addToContainerAt modePicker.gfx origin2D s
    let initState = State.empty assignment fps msgs editor
    let initViewState = ViewState { renderer: r
                                  , actionCh: ch
                                  , stage: s
                                  , fps: fpsView
                                  , msgs: msgsView
                                  , tooltip: tooltipView
                                  , mode: State.mode initState
                                  , editor: editorViewState
                                  , assignment: assignmentViewState
                                  , modePicker: modePicker
                                  , nextEffect: AnyEff (pure unit)
                                  , modal: Nothing
                                  }
    _ <- addModeView EditorMode initViewState
    pure (Tuple initState initViewState)

draw :: State.State -> ViewState -> ViewState
draw state vss@(ViewState viewState) =
  case Modal.draw viewState.stage viewState.actionCh (State.modal state) viewState.modal of
    Tuple modal' (AnyEff modalEff) -> let
      nextEffect :: forall r. PixiChEff r Unit
      nextEffect = do
        _ <- adjustModeView state vss
        _ <- FpsView.draw (State.fps state) viewState.fps
        _ <- MsgsView.draw (State.msgs state) viewState.msgs
        _ <- TooltipView.draw (State.tooltip state) viewState.tooltip
        _ <- if State.updated state
             then do
               _ <- ModePicker.draw (State.mode state) viewState.actionCh viewState.modePicker
               _ <- drawModeView (State.mode state) state vss
               pure unit
             else pure unit
        _ <- modalEff
        _ <- runFn2 renderContainer viewState.stage viewState.renderer
        pure unit
      in ViewState (viewState { modal      = modal'
                              , nextEffect = AnyEff nextEffect
                              , mode       = State.mode state })

adjustModeView :: forall r. State.State -> ViewState -> PixiEff r Unit
adjustModeView state vss@(ViewState vs) | (State.mode state) /= vs.mode = do
  _ <- removeModeView vs.mode vss
  _ <- addModeView (State.mode state) vss
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

drawModeView :: forall r. Mode -> State.State -> ViewState -> PixiChEff r Unit
drawModeView EditorMode state (ViewState vs) =
  EditorMain.draw vs.actionCh (State.editor state) vs.editor
drawModeView AssignmentMode state (ViewState vs) = AssignmentView.draw (State.assignment state) vs.assignment
drawModeView SimulationMode _ _ = pure unit
