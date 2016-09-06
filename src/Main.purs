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
import View.Editor as EditorView
import View.EditorControl as EditorControlView
import View.Fps as FpsView
import View.Messages as MsgsView
import View.Modal as Modal
import View.Tooltip as TooltipView
import Control.Alt ((<|>))
import Data.Either (Either(Left, Right))
import Data.Function.Uncurried (runFn0, runFn2)
import Data.Int (floor)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Tuple (Tuple(Tuple))
import View.Modal (dimap)

newtype State = State
  { fps :: FpsView.FpsState
  , msgs :: MsgsView.MsgsState
  , tooltip :: TooltipView.TooltipState
  , editor :: Editor
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
  , editorView :: EditorView.EditorView
  , editorControlView :: Graphics
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

setup :: forall r. (ChSend Action) -> PixiChEff r (Tuple State ViewState)
setup ch = let
  city     = theCity
  editor   = emptyEditor city
  cityW    = City.width city
  totalW   = cityW + sideBarW
  tooltipH = boxH
  totalH   = City.height city + tooltipH
  r        = runFn2 newRenderer (floor totalW) (floor totalH)
  in do
    _    <- runFn2 setBgColor 0x999999 r
    _    <- appendRendererToBody r
    let s = runFn0 newContainer
    Tuple fps fpsView  <- FpsView.setup s
    Tuple msgs msgsView <- MsgsView.setup s
    editorView <- EditorView.setup ch city (createMap editor)
    _    <- runFn2 addToContainer editorView.btnsLayer s
    _    <- runFn2 addToContainer editorView.gfxLayer  s
    editorControlView <- EditorControlView.setup totalH
    _    <- runFn2 addToContainer editorControlView s
    _    <- runFn2 setPosition { x: cityW, y: 0.0 } editorControlView
    Tuple tooltip tooltipView <- TooltipView.setup totalW tooltipH
    _    <- addToContainerAt tooltipView.gfx { x: 0.0, y: totalH - tooltipH } s
    let initState = { fps: fps
                    , msgs: msgs
                    , tooltip: tooltip
                    , editor: editor
                    , modal: Nothing
                    , updated: true
                    }
    let initViewState = ViewState { renderer: r
                        , actionCh: ch
                        , stage: s
                        , fps: fpsView
                        , msgs: msgsView
                        , tooltip: tooltipView
                        , editorView: editorView
                        , editorControlView: editorControlView
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
step NoOp state = state

updated :: State -> State
updated (State s) = State $ s { updated = true }

draw :: State -> ViewState -> ViewState
draw (State state) (ViewState viewState) =
  case Modal.draw viewState.stage viewState.actionCh state.modal viewState.modal of
    Tuple modal' (AnyEff modalEff) -> let
      tooltip    = (state.tooltip <|> (Just $ editorTooltip state.editor))
      nextEffect :: forall r. PixiChEff r Unit
      nextEffect = do
        _ <- FpsView.draw state.fps viewState.fps
        _ <- MsgsView.draw state.msgs viewState.msgs
        _ <- TooltipView.draw tooltip viewState.tooltip
        _ <- if state.updated      
             then
               EditorView.draw viewState.editorView.gfxLayer state.editor.city (createMap state.editor) *>
               EditorControlView.draw viewState.actionCh viewState.editorControlView state.editor
             else pure unit
        _ <- modalEff
        _ <- runFn2 renderContainer viewState.stage viewState.renderer
        pure unit
      in ViewState (viewState { modal = modal', nextEffect = AnyEff nextEffect })
