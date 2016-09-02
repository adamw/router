module View.Modal
  ( ModalState
  , ModalViewState
  , ModalTexts
  , setup
  , draw
  , update
  ) where

import Data.Coords
import Data.Tuple
import Pixi
import Prelude
import Signal.Channel
import View.Actions
import View.Dimensions
import View.Buttons
import Data.Function.Uncurried (runFn0, runFn2)
import Data.Maybe (Maybe(Nothing, Just))

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
  btnW     = boxH*4.0
  modalH   = boxH*5.0
  modalW   = btnW*2.0+boxH*6.0
  gfx      = runFn0 newGraphics
  bgGfx    = runFn0 newGraphics
  drawModal :: forall r. PixiChEff r Unit
  drawModal = do
    -- components
    txt       <- newTextWithStyle state.texts.prompt (defaultTextStyle { align = "center" })
    okBtn     <- drawButton btnW state.texts.ok     Nothing ch (ModalAction ModalOk)
    cancelBtn <- drawButton btnW state.texts.cancel Nothing ch (ModalAction ModalCancel)
    modalBg   <- withGraphics [ beginFill (Color 0xBBBBBB) opaque
                              , lineStyle (Width 2.0) black opaque
                              , drawRect origin2D modalW modalH
                              , endFill
                              ] bgGfx
    -- container
    cntrW     <- getWidth  cntr
    cntrH     <- getHeight cntr
    _         <- setDim cntrW cntrH gfx
    -- creating a transparent interactive rectangle to capture all mouse events
    -- buttons come later (bigger z index)
    _         <- runFn2 setInteractive true gfx
    _         <- withGraphics [ beginFill black transparent
                              , drawRect origin2D cntrW cntrH
                              , endFill ] gfx
    _         <- addToContainerAt gfx origin2D cntr
    -- adding components
    _         <- setMiddleAnchor txt
    _         <- addToContainerAt bgGfx { x: cntrW/2.0-modalW/2.0, y: cntrH/2.0-modalH/2.0 } gfx
    _         <- addToContainerAt txt { x: cntrW/2.0, y: cntrH/2.0-boxH } gfx
    _         <- addToContainerAt okBtn { x: cntrW/2.0-boxH-btnW, y: cntrH/2.0+boxH } gfx
    _         <- addToContainerAt cancelBtn { x: cntrW/2.0+boxH, y: cntrH/2.0+boxH } gfx
    pure unit
  in Tuple (Just { gfx: gfx }) (AnyEff drawModal)
draw cntr _  Nothing (Just viewState) = Tuple Nothing (AnyEff removeModal) where
  removeModal :: forall r. PixiChEff r Unit
  removeModal = runFn2 removeFromContainer viewState.gfx cntr
draw _ _ _ viewState = Tuple viewState (AnyEff (pure unit))
