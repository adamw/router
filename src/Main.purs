module Main where

import Prelude
import Signal
import Editor
import Pixi
import TheCity
import View.Actions
import View.Dimensions
import ChSend
import City as City
import EditorMain as EditorMain
import Signal.Channel as SignalCh
import Signal.DOM as SignalDOM
import View.Assignment as AssignmentView
import View.Fps as FpsView
import View.Messages as MsgsView
import View.Modal as Modal
import View.Tooltip as TooltipView
import View.ModePicker as ModePicker
import Assignment (emptyAssignment, Assignment)
import Control.Alt ((<|>))
import Data.Coords (origin2D)
import Data.Either (Either(Left, Right))
import Data.Function.Uncurried (runFn0, runFn2)
import Data.Int (floor)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import Modes (Mode(SimulationMode, AssignmentMode, EditorMode))
import View.Modal (dimap)

newtype State = State
  { fps :: FpsView.FpsState
  , msgs :: MsgsView.MsgsState
  , tooltip :: TooltipView.TooltipState
  , mode :: Mode
  , editor :: Editor
  , assignment :: Assignment
  , modal :: Maybe (Modal.ModalState State)
  , updated :: Boolean
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
  , assignment :: Graphics
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
  city     = theCity
  cityW    = City.width city
  totalW   = cityW + sideBarW
  tooltipH = boxH
  totalH   = pickerH + City.height city + tooltipH
  r        = runFn2 newRenderer (floor totalW) (floor totalH)
  assignment = emptyAssignment city
  in do
    _    <- runFn2 setBgColor 0x999999 r
    _    <- appendRendererToBody r
    let s = runFn0 newContainer
    Tuple fps fpsView  <- FpsView.setup s
    Tuple msgs msgsView <- MsgsView.setup s
    Tuple editor editorViewState <- EditorMain.setup ch city totalH
    _    <- addToContainerAt (EditorMain.container editorViewState) modeViewCoords s
    Tuple tooltip tooltipView <- TooltipView.setup totalW tooltipH
    _    <- addToContainerAt tooltipView.gfx { x: 0.0, y: totalH - tooltipH } s
    assignmentViewState <- AssignmentView.setup city
    let modePicker = ModePicker.setup totalW pickerH
    _    <- addToContainerAt modePicker.gfx origin2D s
    let initState = { fps: fps
                    , msgs: msgs
                    , tooltip: tooltip
                    , mode: EditorMode
                    , editor: editor
                    , assignment: assignment
                    , modal: Nothing
                    , updated: true
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
    pure (Tuple (State initState) initViewState)

step :: Action -> State -> State
step (AnimationFrame nowMillis) (State state) = State $ state
  { fps     = FpsView.update (floor (nowMillis / 1000.0)) state.fps
  , updated = false
  }
step (RouteMapAction (Click stopId)) (State state) = State $ state
  { msgs    = MsgsView.update ("You clicked " <> (show stopId)) state.msgs
  , editor  = selectStop stopId state.editor
  , updated = true
  }
step (RouteMapAction (Hover stopId)) (State state) = State $ state
  { msgs    = MsgsView.update ("Hovering " <> (show stopId)) state.msgs
  , editor  = candidateStop stopId state.editor
  , updated = true
  }
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
step (TooltipAction (ShowTooltip tooltip)) (State state) = State $ state
  { msgs    = MsgsView.update ("Show tooltip") state.msgs
  , tooltip = tooltip
  }  
step (TooltipAction ClearTooltip) (State state) = State $ state
  { msgs    = MsgsView.update ("Clear tooltip") state.msgs
  , tooltip = Nothing
  }
step (SwitchToMode m) (State state) = updated $ State $ state { mode = m }
step NoOp state = state

updated :: State -> State
updated (State s) = State $ s { updated = true }

draw :: State -> ViewState -> ViewState
draw ss@(State state) vss@(ViewState viewState) =
  case Modal.draw viewState.stage viewState.actionCh state.modal viewState.modal of
    Tuple modal' (AnyEff modalEff) -> let
      tooltip    = (state.tooltip <|> (Just $ editorTooltip state.editor))
      nextEffect :: forall r. PixiChEff r Unit
      nextEffect = do
        _ <- adjustModeView ss vss
        _ <- FpsView.draw state.fps viewState.fps
        _ <- MsgsView.draw state.msgs viewState.msgs
        _ <- TooltipView.draw tooltip viewState.tooltip
        _ <- if state.updated      
             then do
               _ <- ModePicker.draw state.mode viewState.actionCh viewState.modePicker
               _ <- drawModeView viewState.mode ss vss
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
  addToContainerAt vs.assignment modeViewCoords vs.stage
addModeView SimulationMode (ViewState vs) = pure unit

removeModeView :: forall r. Mode -> ViewState -> PixiEff r Unit
removeModeView EditorMode (ViewState vs)     = removeFromContainer (EditorMain.container vs.editor)
removeModeView AssignmentMode (ViewState vs) = removeFromContainer vs.assignment
removeModeView SimulationMode (ViewState vs) = pure unit

drawModeView :: forall r. Mode -> State -> ViewState -> PixiChEff r Unit
drawModeView EditorMode (State s) (ViewState vs) =
  EditorMain.draw vs.actionCh s.editor vs.editor
drawModeView AssignmentMode (State s) (ViewState vs) = AssignmentView.draw vs.assignment
drawModeView SimulationMode _ _ = pure unit
