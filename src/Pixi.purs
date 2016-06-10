module Pixi where

import Prelude
import Control.Monad.Eff(Eff())
import Data.Function
import Data.Coords
import Data.Foldable(sequence_)

import Signal.Channel(Channel, CHANNEL, send)

foreign import data PIXI :: !

foreign import data Renderer :: *
foreign import data Container :: *                    
foreign import data Text :: *
foreign import data Graphics :: *
                    
foreign import data Circle :: *                    
foreign import data Rectangle :: *                    

type PixiEff r t = Eff (pixi :: PIXI | r) t
type PixiChEff r t = (Eff (channel :: CHANNEL, pixi :: PIXI | r) t)

newtype Color = Color Int

newtype Alpha = Alpha Number
opaque :: Alpha
opaque = Alpha 1.0

newtype Width = Width Number

foreign import newRenderer    :: forall r. Fn2 Int Int (PixiEff r Renderer)
foreign import newContainer   :: forall r. PixiEff r Container
foreign import newText        :: forall r. PixiEff r Text
foreign import newGraphics    :: forall r. PixiEff r Graphics

foreign import newCircle      :: forall r. Fn2 Coords Number (PixiEff r Circle)
foreign import newRectangle   :: forall r. Fn3 Coords Number Number (PixiEff r Rectangle)

foreign import appendRendererToBody :: forall r. Renderer -> PixiEff r Unit
foreign import renderContainer :: forall t r. (IsCntr t) => Fn2 t Renderer (PixiEff r Unit)

foreign import setBgColor     :: forall r. Fn2 Int Renderer (PixiEff r Unit)
foreign import setText        :: forall r. Fn2 String Text (PixiEff r Unit)
foreign import setTextStyle   :: forall a r. Fn2 { | a } Text (PixiEff r Unit)

smallTextStyle = { font: "12px Arial" }
defaultTextStyle = { font: "bold 20px Arial" }

newTextWithStyle :: forall r a. String -> { | a } -> (PixiEff r Text)
newTextWithStyle text style = do
  t <-        newText
  _ <- runFn2 setText text t
  _ <- runFn2 setTextStyle style t
  return t

--
-- Common properties
--
  
foreign import setPosition    :: forall t r. (IsDisObj t) => Fn2 Coords t (PixiEff r Unit)
foreign import setRotation    :: forall t r. (IsDisObj t) => Fn2 Number t (PixiEff r Unit)
foreign import setWidth       :: forall t r. (IsDisObj t) => Fn2 Number t (PixiEff r Unit)
foreign import setHeight      :: forall t r. (IsDisObj t) => Fn2 Number t (PixiEff r Unit)
foreign import setAnchor      :: forall t r. (IsDisObj t) => Fn3 Number Number t (PixiEff r Unit)

foreign import setInteractive :: forall o r. (IsDisObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setButtonMode  :: forall o r. (IsDisObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setHitArea     :: forall s o r. (IsShape s, IsDisObj o) => Fn2 s o (PixiEff r Unit)

--
-- Containers
-- 

foreign import addToContainer :: forall o c r. (IsDisObj o, IsCntr c) => Fn2 o c (PixiEff r Unit)
foreign import removeAllFromContainer :: forall c r. (IsCntr c) => c -> (PixiEff r Unit)

addToContainerAt :: forall o c r. (IsDisObj o, IsCntr c) => o -> Coords -> c -> (PixiEff r Unit)
addToContainerAt obj coords cnt = do
  _ <- runFn2 addToContainer obj cnt
  runFn2 setPosition coords obj

-- 
-- Graphics
--
  
foreign import clear          :: forall r.                          Graphics -> (PixiEff r Unit)
foreign import _beginFill     :: forall r. Fn3 Color Alpha          Graphics (PixiEff r Unit)
foreign import _lineStyle     :: forall r. Fn4 Width Color Alpha    Graphics (PixiEff r Unit)
foreign import _drawCircle    :: forall r. Fn3 Coords Number        Graphics (PixiEff r Unit)
foreign import _drawRect      :: forall r. Fn4 Coords Number Number Graphics (PixiEff r Unit)
foreign import _moveTo        :: forall r. Fn2 Coords               Graphics (PixiEff r Unit)
foreign import _lineTo        :: forall r. Fn2 Coords               Graphics (PixiEff r Unit)
foreign import _arc           :: forall r. Fn5 Coords Number Number Number Graphics (PixiEff r Unit)
foreign import endFill        :: forall r.                          Graphics -> (PixiEff r Unit)

beginFill :: forall r. Color -> Alpha -> Graphics -> PixiEff r Unit
beginFill c a g = runFn3 _beginFill c a g

lineStyle :: forall r. Width -> Color -> Alpha -> Graphics -> PixiEff r Unit
lineStyle w c a g = runFn4 _lineStyle w c a g

drawCircle :: forall r. Coords -> Number -> Graphics -> PixiEff r Unit
drawCircle c n g = runFn3 _drawCircle c n g

drawRect :: forall r. Coords -> Number -> Number -> Graphics -> PixiEff r Unit
drawRect c n1 n2 g = runFn4 _drawRect c n1 n2 g

moveTo :: forall r. Coords -> Graphics -> PixiEff r Unit
moveTo c g = runFn2 _moveTo c g

lineTo :: forall r. Coords -> Graphics -> PixiEff r Unit
lineTo c g = runFn2 _lineTo c g

arc :: forall r. Coords -> Number -> Number -> Number -> Graphics -> PixiEff r Unit
arc c n1 n2 n3 g = runFn5 _arc c n1 n2 n3 g

withGraphics :: forall r. Array (Graphics -> PixiEff r Unit) -> Graphics -> PixiEff r Unit
withGraphics a g = sequence_ $ (_ $ g) <$> a

--
-- Events
-- 

foreign import _onMouseDown   :: forall o r. Fn2 (Eff (channel :: CHANNEL | r) Unit) o (PixiChEff r Unit)
foreign import _onMouseOver   :: forall o r. Fn2 (Eff (channel :: CHANNEL | r) Unit) o (PixiChEff r Unit)
foreign import _onMouseOut   :: forall o r. Fn2 (Eff (channel :: CHANNEL | r) Unit) o (PixiChEff r Unit)

onMouseDown :: forall a o r. (IsDisObj o) => (Channel a) -> a -> o -> (PixiChEff r Unit)
onMouseDown ch msg obj = runFn2 _onMouseDown (send ch msg) obj

onMouseHover :: forall a o r. (IsDisObj o) => (Channel a) -> a -> a -> o -> (PixiChEff r Unit)
onMouseHover ch msgIn msgOut obj = do
  _ <- runFn2 _onMouseOver (send ch msgIn) obj
  _ <- runFn2 _onMouseOut (send ch msgOut) obj
  return unit

--
-- Typeclasses
-- 

class IsDisObj o
instance containerIsDisplayObject :: IsDisObj Container
instance textIsDisplayObject      :: IsDisObj Text
instance graphicsIsDisplayObject  :: IsDisObj Graphics

class IsCntr t
instance containerIsContainer :: IsCntr Container
instance graphicsIsContainer :: IsCntr Graphics

class IsShape c
instance circleIsShape :: IsShape Circle
instance rectangleIsShape :: IsShape Rectangle
