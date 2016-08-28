module View.Modal
  ( ModalState
  , ModalViewState
  , ModalTexts
  , setup
  , draw
  , update
  ) where

import Data.Coords
import Data.Function.Uncurried(runFn0, runFn2)
import Data.Tuple
import Pixi
import Prelude
import Signal.Channel
import View.Actions
import View.Dimensions
import View.Buttons
import Data.Maybe (Maybe(Nothing, Just))
import Debug.Trace (spy)

newtype ModalState t = ModalState { texts :: ModalTexts
                                  , f :: t -> t }

type ModalViewState = { gfx :: Graphics }

type ModalTexts = { prompt :: String
                  , ok     :: String
                  , cancel :: String
                  }

setup :: forall t. ModalTexts -> (t -> t) -> ModalState t
setup texts f = ModalState { texts: texts, f: f }

update :: forall t. Maybe (ModalState t) -> ModalAction -> t -> Tuple t (Maybe (ModalState t))
update Nothing                   _           t = Tuple t           Nothing
update (Just (ModalState modal)) ModalOk     t = Tuple (modal.f t) Nothing
update (Just modal)              ModalCancel t = Tuple t           Nothing

draw :: forall c t. (IsCntr c, IsDisObj c) => c -> Channel Action -> Maybe (ModalState t) -> Maybe ModalViewState -> Tuple (Maybe ModalViewState) AnyEff
draw cntr ch (Just (ModalState state)) Nothing = let
  width = boxH*4.0
  gfx = runFn0 newGraphics
  drawModal :: forall r. PixiChEff r Unit
  drawModal = do
    -- components
    txt       <- newTextWithStyle state.texts.prompt defaultTextStyle
    okBtn     <- drawButton width state.texts.ok     ch (ModalAction ModalOk)
    cancelBtn <- drawButton width state.texts.cancel ch (ModalAction ModalCancel)
    -- container
    cntrW     <- getWidth  cntr
    cntrH     <- getHeight cntr
    _         <- runFn2 setWidth  cntrW gfx
    _         <- runFn2 setHeight cntrH gfx
    _         <- addToContainerAt gfx origin2D cntr
    -- adding components
    _         <- setMiddleAnchor txt
    _         <- addToContainerAt txt { x: cntrW/2.0, y: cntrH/2.0 } gfx
--    _         <- setMiddleAnchor okBtn
--    xxx <- getHeight okBtn
--    let zzz = spy xxx
    _         <- addToContainerAt okBtn { x: cntrW/2.0-boxH*3.0, y: cntrH/2.0+boxH*3.0 } gfx
--    _         <- setMiddleAnchor cancelBtn
    _         <- addToContainerAt okBtn { x: cntrW/2.0+boxH*3.0, y: cntrH/2.0+boxH*3.0 } gfx
    pure unit
  in Tuple (Just { gfx: gfx }) (AnyEff drawModal)
draw cntr _  Nothing (Just viewState) = Tuple Nothing (AnyEff removeModal) where
  removeModal :: forall r. PixiChEff r Unit
  removeModal = runFn2 removeFromContainer viewState.gfx cntr
draw _ _ _ viewState = Tuple viewState (AnyEff (pure unit))
