module EditorMain where

import Editor
import Data.Either (Either(Left, Right))
import View.Actions (EditorAction(EditRoute, RemoveRoute, RemoveLastStop, CompleteRoute))
import View.Modal (ModalState, setup) as Modal

step :: EditorAction -> Editor -> Either (Modal.ModalState Editor) Editor
step CompleteRoute editor = Right (finishRoute editor)
step RemoveLastStop editor = Right (removeLastStop editor) 
step (RemoveRoute routeId) editor = Left modal where 
  modal = Modal.setup
    { prompt: "Are you sure you want to remove\nthis route?", ok: "Yes", cancel: "No" }
    (deleteRoute routeId)
step (EditRoute routeId) editor = Right (editRoute routeId editor)
