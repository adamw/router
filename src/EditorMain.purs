module EditorMain
  ( step
  , setup
  , container
  , draw
  , module View.WithControl) where

import Prelude
import Editor
import Pixi
import ChSend (ChSend)
import City (City)
import Data.Either (Either(Left, Right))
import Data.Functor.Contravariant ((>$<))
import View.Modal (ModalState, setup) as Modal
import View.EditorControl as EditorControlView
import View.RoutesMap as RoutesMapView
import View.Actions (Action, EditorAction(EditRoute, RemoveRoute, RemoveLastStop, CompleteRoute))
import View.WithControl 

step :: EditorAction -> Editor -> Either (Modal.ModalState Editor) Editor
step CompleteRoute editor = Right (finishRoute editor)
step RemoveLastStop editor = Right (removeLastStop editor) 
step (RemoveRoute routeId) editor = Left modal where 
  modal = Modal.setup
    { prompt: "Are you sure you want to remove\nthis route?", ok: "Yes", cancel: "No" }
    (deleteRoute routeId)
step (EditRoute routeId) editor = Right (editRoute routeId editor)

setup :: forall r. ChSend Action -> City -> Number -> PixiChEff r ViewState
setup ch city height = do
  vs@(ViewState viewState) <- setupWithControl ch city height
  _ <- setupMapButtons vs ch city
  pure vs
      
container :: ViewState -> Container
container (ViewState { main: main }) = main

draw :: forall r. (ChSend Action) -> Editor -> ViewState -> PixiChEff r Unit
draw ch editor (ViewState viewState) =
  RoutesMapView.draw viewState.map editor.city (createMap editor) *>
  EditorControlView.draw ch viewState.control editor
