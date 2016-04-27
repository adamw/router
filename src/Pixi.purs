module Pixi where

import Prelude
import Control.Monad.Eff (Eff())
import Data.Function

import Signal.Channel (Channel, CHANNEL, send)

foreign import data PIXI :: !

foreign import data Renderer :: *
foreign import data Container :: *                    
foreign import data Text :: *
foreign import data Graphics :: *
                    
foreign import data Circle :: *                    

type PixiEff r t = Eff (pixi :: PIXI | r) t
type PixiChEff r t = (Eff (channel :: CHANNEL, pixi :: PIXI | r) t)

foreign import newRenderer :: forall r. Fn2 Int Int (PixiEff r Renderer)
foreign import newContainer :: forall r. Fn0 (PixiEff r Container)
foreign import newText :: forall r. Fn0 (PixiEff r Text)
foreign import newGraphics :: forall r. Fn0 (PixiEff r Graphics)

foreign import newCircle :: forall r. Fn3 Number Number Number (PixiEff r Circle)

foreign import appendRendererToBody :: forall r. Renderer -> PixiEff r Unit
foreign import renderContainer :: forall t r. (IsContainer t) => Fn2 t Renderer (PixiEff r Unit)

foreign import setBackgroundColor :: forall r. Fn2 Int Renderer (PixiEff r Unit)
foreign import setText :: forall r. Fn2 String Text (PixiEff r Unit)

foreign import setPosition :: forall t r. (IsDisplayObj t) => Fn3 Number Number t (PixiEff r Unit)

foreign import setInteractive :: forall o r. (IsDisplayObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setButtonMode :: forall o r. (IsDisplayObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setHitArea :: forall s o r. (IsShape s, IsDisplayObj o) => Fn2 s o (PixiEff r Unit)

foreign import addToContainer :: forall c t r. (IsDisplayObj c, IsContainer t) => Fn2 c t (PixiEff r Unit)

-- TODO: Color -> newtype + int
foreign import beginFill  :: forall r. Fn3 Number Number        Graphics (PixiEff r Graphics)
foreign import lineStyle  :: forall r. Fn4 Number Number Number Graphics (PixiEff r Graphics)
foreign import drawCircle :: forall r. Fn4 Number Number Number Graphics (PixiEff r Graphics)
foreign import endFill    :: forall r. Fn1                      Graphics (PixiEff r Graphics)

foreign import _onMouseDown :: forall o r. Fn2 (Eff (channel :: CHANNEL | r) Unit) o (PixiChEff r Unit)

onMouseDown :: forall a o r. (IsDisplayObj o) => (Channel a) -> a -> o -> (PixiChEff r Unit)
onMouseDown ch msg obj = runFn2 _onMouseDown (send ch msg) obj

class IsDisplayObj o
instance containerIsDisplayObject :: IsDisplayObj Container
instance textIsDisplayObject :: IsDisplayObj Text
instance graphicsIsDisplayObject :: IsDisplayObj Graphics

class IsContainer t
instance containerIsContainer :: IsContainer Container

class IsShape c
instance circleIsShape :: IsShape Circle
