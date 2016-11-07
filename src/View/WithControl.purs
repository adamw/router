module View.WithControl
  ( ViewState(..)
  , setupWithControl
  , setupMapButtons
  , boxBorderColor
  ) where

import Prelude
import Pixi
import View.RoutesMap as RoutesMapView
import ChSend (ChSend)
import City (width, City)
import Data.Coords (origin2D)
import Data.Function.Uncurried (runFn2, runFn0)
import Data.Functor.Contravariant ((>$<))
import View.Actions (Action(RouteMapAction))

newtype ViewState = ViewState { main    :: Container
                              , map     :: Graphics
                              , control :: Graphics
                              }

setupWithControl :: forall r. (ChSend Action) -> City -> Number -> PixiEff r ViewState
setupWithControl ch city height = let
 cityW    = width city
 main     = runFn0 newContainer
 map      = runFn0 newGraphics
    in do
      _       <- runFn2 addToContainer map main
      control <-        setupControl height
      _       <- runFn2 addToContainer control main
      _       <- runFn2 setPosition { x: cityW, y: 0.0 } control
      pure $ ViewState { main: main
                       , map: map
                       , control: control }

setupMapButtons :: forall r. ViewState -> ChSend Action -> City -> PixiChEff r Unit
setupMapButtons (ViewState vs) ch city = do
  btns <- RoutesMapView.setupButtons (RouteMapAction >$< ch) city
  _    <- runFn2 addToContainer btns vs.main
  pure unit

setupControl :: forall t. Number -> PixiEff t Graphics
setupControl height = let control = runFn0 newGraphics in do
  _ <- withGraphics [ lineStyle (Width 1.0) boxBorderColor opaque
                    , moveTo origin2D
                    , lineTo { x: 0.0, y: height }  
                    ] control
  pure control

boxBorderColor :: Color
boxBorderColor = Color 0x555555
