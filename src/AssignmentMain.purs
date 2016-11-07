module AssignmentMain
  ( step
  , setup
  , container
  , draw
  , module View.WithControl) where

import Pixi
import Prelude
import Data.Set as S
import RoutesMap as RoutesMap
import View.RoutesMap as RoutesMapView
import Assignment (removeBus, addBus, Assignment)
import ChSend (ChSend)
import City (City)
import Data.Functor.Contravariant ((>$<))
import View.Actions (Action, AssignmentAction(RemoveBus, AddBus))
import View.AssignmentControl as AssignmentControlView
import View.WithControl 

step :: AssignmentAction -> Assignment -> Assignment
step (AddBus routeId)    = addBus routeId
step (RemoveBus routeId) = removeBus routeId

setup :: forall r. ChSend Action -> City -> Number -> PixiChEff r ViewState
setup ch city height = do
  vs@(ViewState viewState) <- setupWithControl ch city height
  _ <- setupMapButtons vs ch city
  pure vs
  
container :: ViewState -> Container
container (ViewState vs) = vs.main

draw :: forall t. ChSend Action -> Assignment -> ViewState -> PixiChEff t Unit
draw ch a (ViewState vs) =
  RoutesMapView.draw vs.map a.city (RoutesMap.create a.routes (S.fromFoldable a.selectedStop)) *>
  AssignmentControlView.draw ch vs.control a

