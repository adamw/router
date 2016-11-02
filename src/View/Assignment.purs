module View.Assignment
  ( ViewState
  , setup
  , draw
  , container
  ) where

import Pixi
import Prelude
import View.RoutesMap as RoutesMapView
import Assignment (Assignment)
import ChSend (ChSend)
import City (City)
import Data.Function.Uncurried (runFn2, runFn0)
import Data.Functor.Contravariant ((>$<))
import View.Actions (Action(RouteMapAction))

newtype ViewState = ViewState { main :: Container
                              , gfx :: Graphics
                              }

setup :: forall r. (ChSend Action) -> City -> PixiChEff r ViewState
setup ch city = let
  c   = runFn0 newContainer
  gfx = runFn0 newGraphics
  in do
    btns <- RoutesMapView.setupButtons (RouteMapAction >$< ch) city
    _    <- runFn2 addToContainer btns c
    _    <- runFn2 addToContainer gfx  c
    pure $ ViewState { main: c
                     , gfx:  gfx
                     }

draw :: forall t. Assignment -> ViewState -> PixiEff t Unit
draw a (ViewState vs) = RoutesMapView.draw vs.gfx a.city a.routesMap 

container :: ViewState -> Container
container (ViewState vs) = vs.main
