module Pixi where

import Prelude
import Control.Monad.Eff (Eff())
import Data.Function
import Data.Coords

import Signal.Channel (Channel, CHANNEL, send)

foreign import data PIXI :: !

foreign import data Renderer :: *
foreign import data Container :: *                    
foreign import data Text :: *
foreign import data Graphics :: *
                    
foreign import data Circle :: *                    

type PixiEff r t = Eff (pixi :: PIXI | r) t
type PixiChEff r t = (Eff (channel :: CHANNEL, pixi :: PIXI | r) t)

newtype Color = Color Int

newtype Alpha = Alpha Number
opaque :: Alpha
opaque = Alpha 1.0

newtype Width = Width Number

foreign import newRenderer    :: forall r. Fn2 Int Int (PixiEff r Renderer)
foreign import newContainer   :: forall r. Fn0 (PixiEff r Container)
foreign import newText        :: forall r. Fn0 (PixiEff r Text)
foreign import newGraphics    :: forall r. Fn0 (PixiEff r Graphics)

foreign import newCircle      :: forall r. Fn2 Coords Number (PixiEff r Circle)

foreign import appendRendererToBody :: forall r. Renderer -> PixiEff r Unit
foreign import renderContainer :: forall t r. (IsCntr t) => Fn2 t Renderer (PixiEff r Unit)

foreign import setBgColor     :: forall r. Fn2 Int Renderer (PixiEff r Unit)
foreign import setText        :: forall r. Fn2 String Text (PixiEff r Unit)
foreign import setTextStyle   :: forall a r. Fn2 { | a } Text (PixiEff r Unit)

foreign import setPosition    :: forall t r. (IsDisObj t) => Fn2 Coords t (PixiEff r Unit)
foreign import setRotation    :: forall t r. (IsDisObj t) => Fn2 Number t (PixiEff r Unit)
foreign import setWidth       :: forall t r. (IsDisObj t) => Fn2 Number t (PixiEff r Unit)

foreign import setInteractive :: forall o r. (IsDisObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setButtonMode  :: forall o r. (IsDisObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setHitArea     :: forall s o r. (IsShape s, IsDisObj o) => Fn2 s o (PixiEff r Unit)

foreign import addToContainer :: forall o c r. (IsDisObj o, IsCntr c) => Fn2 o c (PixiEff r Unit)
foreign import removeAllFromContainer :: forall c r. (IsCntr c) => Fn1 c (PixiEff r Unit)

foreign import clear          :: forall r. Fn1                      Graphics (PixiEff r Unit)
foreign import beginFill      :: forall r. Fn3 Color Alpha          Graphics (PixiEff r Unit)
foreign import lineStyle      :: forall r. Fn4 Width Color Alpha    Graphics (PixiEff r Unit)
foreign import drawCircle     :: forall r. Fn3 Coords Number        Graphics (PixiEff r Unit)
foreign import drawRect       :: forall r. Fn4 Coords Number Number Graphics (PixiEff r Unit)
foreign import moveTo         :: forall r. Fn2 Coords               Graphics (PixiEff r Unit)
foreign import lineTo         :: forall r. Fn2 Coords               Graphics (PixiEff r Unit)
foreign import arc            :: forall r. Fn5 Coords Number Number Number Graphics (PixiEff r Unit)
foreign import endFill        :: forall r. Fn1                      Graphics (PixiEff r Unit)

foreign import _onMouseDown   :: forall o r. Fn2 (Eff (channel :: CHANNEL | r) Unit) o (PixiChEff r Unit)
foreign import _onMouseOver   :: forall o r. Fn2 (Eff (channel :: CHANNEL | r) Unit) o (PixiChEff r Unit)
foreign import _onMouseOut   :: forall o r. Fn2 (Eff (channel :: CHANNEL | r) Unit) o (PixiChEff r Unit)

smallTextStyle = { font: "12px Arial" }
defaultTextStyle = { font: "bold 20px Arial" }

newTextWithStyle :: forall r a. String -> { | a } -> (PixiEff r Text)
newTextWithStyle text style = do
  t <- runFn0 newText
  _ <- runFn2 setText text t
  _ <- runFn2 setTextStyle style t
  return t

addToContainerAt :: forall o c r. (IsDisObj o, IsCntr c) => o -> Coords -> c -> (PixiEff r Unit)
addToContainerAt obj coords cnt = do
  _ <- runFn2 addToContainer obj cnt
  runFn2 setPosition coords obj

onMouseDown :: forall a o r. (IsDisObj o) => (Channel a) -> a -> o -> (PixiChEff r Unit)
onMouseDown ch msg obj = runFn2 _onMouseDown (send ch msg) obj

onMouseHover :: forall a o r. (IsDisObj o) => (Channel a) -> a -> a -> o -> (PixiChEff r Unit)
onMouseHover ch msgIn msgOut obj = do
  _ <- runFn2 _onMouseOver (send ch msgIn) obj
  _ <- runFn2 _onMouseOut (send ch msgOut) obj
  return unit

class IsDisObj o
instance containerIsDisplayObject :: IsDisObj Container
instance textIsDisplayObject      :: IsDisObj Text
instance graphicsIsDisplayObject  :: IsDisObj Graphics

class IsCntr t
instance containerIsContainer :: IsCntr Container
instance graphicsIsContainer :: IsCntr Graphics

class IsShape c
instance circleIsShape :: IsShape Circle
