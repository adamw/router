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

type ViewState =
  { renderer :: Renderer
  , actionCh :: SignalCh.Channel Action
  , stage :: Container
  , fps :: FpsView.Fps
  , editor :: Editor
  , editorView :: EditorView.EditorView
  , editorControlView :: Graphics
  , msgs :: MsgsView.Msgs
  , updated :: Boolean
  }

main = do
  actionCh     <- SignalCh.channel NoOp
  initialState <- setup actionCh
  animationSig <- SignalDOM.animationFrame
  let mainSig   = merge (SignalCh.subscribe actionCh) (AnimationFrame <$> animationSig)
  let stepSig   = foldp step initialState mainSig
  let renderSig = render <$> stepSig
  runSignal renderSig

setup :: forall r. (SignalCh.Channel Action) -> PixiChEff r ViewState
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
    fps  <- FpsView.setup s
    msgs <- MsgsView.setup s
    editorView <- EditorView.setup ch city (createMap editor)
    _    <- runFn2 addToContainer editorView.btnsLayer s
    _    <- runFn2 addToContainer editorView.gfxLayer  s
    editorControlView <- EditorControlView.setup totalH
    _    <- runFn2 addToContainer editorControlView s
    _    <- runFn2 setPosition { x: cityW, y: 0.0 } editorControlView
    return { renderer: r
           , actionCh: ch
           , stage: s
           , fps: fps
           , editor: editor
           , editorView: editorView
           , editorControlView: editorControlView
           , msgs: msgs
           , updated: true }

step :: Action -> ViewState -> ViewState
step (AnimationFrame nowMillis) state = state
  { fps     = FpsView.update (floor (nowMillis / 1000.0)) state.fps
  , updated = false
  }
step (Click stopId) state = state
  { msgs    = MsgsView.update ("You clicked " ++ (show stopId)) state.msgs
  , editor  = selectStop stopId state.editor
  , updated = true
  }
step (Hover stopId) state = state
  { msgs    = MsgsView.update ("Hovering " ++ (show stopId)) state.msgs
  , editor  = candidateStop stopId state.editor
  , updated = true
  }
step CompleteRoute state = state
  { msgs    = MsgsView.update ("Complete route") state.msgs
  , editor  = finishRoute state.editor
  , updated = true                  
  }
step RemoveLastStop state = state
  { msgs    = MsgsView.update ("Remove last") state.msgs
  , editor  = removeLastStop state.editor
  , updated = true                  
  }
step (RemoveRoute routeId) state = state
  { msgs    = MsgsView.update ("Delete") state.msgs
  , editor  = deleteRoute routeId state.editor
  , updated = true                  
  }
step (EditRoute routeId) state = state
  { msgs    = MsgsView.update ("Edit") state.msgs
  , editor  = editRoute routeId state.editor
  , updated = true                  
  }
 -- case state.editor.editedRoute.state of
 -- SelectFirst -> 
 -- _ -> state
step NoOp state = state

render :: forall r. ViewState -> PixiChEff r Unit
render state = do
  _ <- FpsView.render state.fps
  _ <- MsgsView.render state.msgs
  _ <- if state.updated      
       then
         EditorView.draw state.editorView.gfxLayer state.editor.city (createMap state.editor) *>
         EditorControlView.draw state.actionCh state.editorControlView state.editor
       else return unit
  _ <- runFn2 renderContainer state.stage state.renderer
  return unit
