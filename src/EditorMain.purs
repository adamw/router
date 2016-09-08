module EditorMain(ViewState, step, setup, container, draw) where

import Prelude
import Editor
import Pixi
import View.Editor as EditorView
import ChSend (ChSend)
import City (City, width)
import Data.Either (Either(Left, Right))
import Data.Function.Uncurried (runFn2, runFn0)
import Data.Tuple (Tuple(Tuple))
import View.Actions (Action, EditorAction(EditRoute, RemoveRoute, RemoveLastStop, CompleteRoute))
import View.Modal (ModalState, setup) as Modal
import View.EditorControl as EditorControlView

newtype ViewState = ViewState { main :: Container
                              , editor :: Graphics
                              , control :: Graphics
                              }

step :: EditorAction -> Editor -> Either (Modal.ModalState Editor) Editor
step CompleteRoute editor = Right (finishRoute editor)
step RemoveLastStop editor = Right (removeLastStop editor) 
step (RemoveRoute routeId) editor = Left modal where 
  modal = Modal.setup
    { prompt: "Are you sure you want to remove\nthis route?", ok: "Yes", cancel: "No" }
    (deleteRoute routeId)
step (EditRoute routeId) editor = Right (editRoute routeId editor)

setup :: forall r. (ChSend Action) -> City -> Number -> PixiChEff r (Tuple Editor ViewState)
setup ch city height = let
    editor   = emptyEditor city
    cityW    = width city
    c        = runFn0 newContainer
    in do
      editorView <- EditorView.setup ch city
      _    <- runFn2 addToContainer editorView.btns c
      _    <- runFn2 addToContainer editorView.gfx  c
      editorControlView <- EditorControlView.setup height
      _    <- runFn2 addToContainer editorControlView c
      _    <- runFn2 setPosition { x: cityW, y: 0.0 } editorControlView
      let initViewState = ViewState { main: c
                                    , editor: editorView.gfx
                                    , control: editorControlView }
      pure $ Tuple editor initViewState

container :: ViewState -> Container
container (ViewState { main: main }) = main

draw :: forall r. (ChSend Action) -> Editor -> ViewState -> PixiChEff r Unit
draw ch editor (ViewState viewState) =
  EditorView.draw viewState.editor editor.city (createMap editor) *>
  EditorControlView.draw ch viewState.control editor
