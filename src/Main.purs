module Main where

import Prelude
import Data.Function
import Control.Apply
import Signal
import Editor
import Pixi
import TheCity
import View.Actions
import View.Dimensions
import City as City
import Signal.Channel as SignalCh
import Signal.DOM as SignalDOM
import View.Editor as EditorView
import View.EditorControl as EditorControlView
import View.Fps as FpsView
import View.Messages as MsgsView
import Data.Int (floor)
import Data.Tuple (Tuple(Tuple))

newtype State = State
  { fps :: FpsView.FpsState
  , msgs :: MsgsView.MsgsState
  , editor :: Editor
  , updated :: Boolean
  }

newtype AnyEff = AnyEff (forall r. PixiChEff r Unit)

newtype ViewState = ViewState
  { renderer :: Renderer
  , actionCh :: SignalCh.Channel Action
  , stage :: Container
  , fps :: FpsView.FpsViewState
  , msgs :: MsgsView.MsgsViewState
  , editorView :: EditorView.EditorView
  , editorControlView :: Graphics
  , nextEffect :: AnyEff
  }

main = do
  actionCh     <- SignalCh.channel NoOp
  Tuple initState initViewState <- setup actionCh
  animationSig <- SignalDOM.animationFrame
  let mainSig   = merge (SignalCh.subscribe actionCh) (AnimationFrame <$> animationSig)
  let stepSig   = foldp step initState mainSig
  let viewStepSig = foldp draw initViewState stepSig
  let nextEff :: ViewState -> forall r. PixiChEff r Unit
      nextEff (ViewState { nextEffect = (AnyEff nextEffect) }) = nextEffect
  let effectSig = nextEff <$> viewStepSig
  runSignal effectSig

setup :: forall r. (SignalCh.Channel Action) -> PixiChEff r (Tuple State ViewState)
setup ch = let
  city   = theCity
  editor = emptyEditor city
  cityW  = City.width city
  totalW = cityW + sideBarW
  totalH = City.height city
  in do
    r    <- runFn2 newRenderer (floor totalW) (floor totalH)
    _    <- runFn2 setBgColor 0x999999 r
    _    <- appendRendererToBody r
    s    <- newContainer
    Tuple fps fpsView  <- FpsView.setup s
    Tuple msgs msgsView <- MsgsView.setup s
    editorView <- EditorView.setup ch city (createMap editor)
    _    <- runFn2 addToContainer editorView.btnsLayer s
    _    <- runFn2 addToContainer editorView.gfxLayer  s
    editorControlView <- EditorControlView.setup totalH
    _    <- runFn2 addToContainer editorControlView s
    _    <- runFn2 setPosition { x: cityW, y: 0.0 } editorControlView
    let initState = { editor: editor
                    , fps: fps
                    , msgs: msgs
                    , updated: true
                    }
    let initViewState = ViewState { renderer: r
                        , actionCh: ch
                        , stage: s
                        , fps: fpsView
                        , msgs: msgsView
                        , editorView: editorView
                        , editorControlView: editorControlView
                        , nextEffect: AnyEff (pure unit)
                        }
    pure (Tuple (State initState) initViewState)

step :: Action -> State -> State
step (AnimationFrame nowMillis) (State state) = State $ state
  { fps     = FpsView.update (floor (nowMillis / 1000.0)) state.fps
  , updated = false
  }
step (Click stopId) (State state) = State $ state
  { msgs    = MsgsView.update ("You clicked " ++ (show stopId)) state.msgs
  , editor  = selectStop stopId state.editor
  , updated = true
  }
step (Hover stopId) (State state) = State $ state
  { msgs    = MsgsView.update ("Hovering " ++ (show stopId)) state.msgs
  , editor  = candidateStop stopId state.editor
  , updated = true
  }
step CompleteRoute (State state) = State $ state
  { msgs    = MsgsView.update ("Complete route") state.msgs
  , editor  = finishRoute state.editor
  , updated = true                  
  }
step RemoveLastStop (State state) = State $ state
  { msgs    = MsgsView.update ("Remove last") state.msgs
  , editor  = removeLastStop state.editor
  , updated = true                  
  }
step (RemoveRoute routeId) (State state) = State $ state
  { msgs    = MsgsView.update ("Delete") state.msgs
  , editor  = deleteRoute routeId state.editor
  , updated = true                  
  }
step (EditRoute routeId) (State state) = State $ state
  { msgs    = MsgsView.update ("Edit") state.msgs
  , editor  = editRoute routeId state.editor
  , updated = true                  
  }
step NoOp state = state

draw :: State -> ViewState -> ViewState
draw (State state) (ViewState viewState) = let
  nextEffect :: forall r. PixiChEff r Unit
  nextEffect = do
    _ <- FpsView.draw state.fps viewState.fps
    _ <- MsgsView.draw state.msgs viewState.msgs
    _ <- if state.updated      
         then
           EditorView.draw viewState.editorView.gfxLayer state.editor.city (createMap state.editor) *>
           EditorControlView.draw viewState.actionCh viewState.editorControlView state.editor
         else return unit
    _ <- runFn2 renderContainer viewState.stage viewState.renderer
    return unit
  in ViewState (viewState { nextEffect = AnyEff nextEffect })
