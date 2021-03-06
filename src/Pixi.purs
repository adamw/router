module Pixi where

import Prelude
import Data.Coords
import ChSend
import Control.Monad.Eff (Eff, kind Effect)
import Data.Foldable (sequence_)
import Data.Function.Uncurried (Fn1, runFn1, Fn0, Fn2, Fn3, Fn4, Fn5, runFn0, runFn2, runFn3, runFn4, runFn5)
import Signal.Channel (CHANNEL)

foreign import data PIXI :: Effect

foreign import data Renderer :: Type
foreign import data Container :: Type                  
foreign import data Text :: Type
foreign import data Graphics :: Type
                    
foreign import data Circle :: Type
foreign import data Rectangle :: Type                    

type PixiEff r t = Eff (pixi :: PIXI | r) t
type PixiChEff r t = (Eff (channel :: CHANNEL, pixi :: PIXI | r) t)

newtype AnyEff = AnyEff (forall r. PixiChEff r Unit)

newtype Color = Color Int

black = Color 0
white = Color 0xFFFFFF

newtype Alpha = Alpha Number

opaque :: Alpha
opaque = Alpha 1.0

transparent :: Alpha
transparent = Alpha 0.0

newtype Width = Width Number

foreign import newRenderer    :: Fn2 Int Int Renderer
foreign import newContainer   :: Fn0 Container
foreign import newText        :: Fn0 Text
foreign import newGraphics    :: Fn0 Graphics

foreign import newCircle      :: forall r. Fn2 Coords Number (PixiEff r Circle)
foreign import newRectangle   :: forall r. Fn3 Coords Number Number (PixiEff r Rectangle)

foreign import appendRendererToBody :: forall r. Renderer -> PixiEff r Unit
foreign import renderContainer :: forall t r. (IsCntr t) => Fn2 t Renderer (PixiEff r Unit)

foreign import setBgColor     :: forall r. Fn2 Int Renderer (PixiEff r Unit)
foreign import setText        :: forall r. Fn2 String Text (PixiEff r Unit)
foreign import setTextStyle   :: forall a r. Fn2 { | a } Text (PixiEff r Unit)
foreign import setAnchor      :: forall r. Fn3 Number Number Text (PixiEff r Unit)

smallTextStyle =
  { fontFamily: "Arial"
  , fontSize: "12px"
  , align: "left"
  , fill: "black"
  }
defaultTextStyle =
  { fontFamily: "Arial"
  , fontSize: "20px"
  , fontWeight: "bold"
  , align: "left"
  , fill: "black "}

newTextWithStyle :: forall r a. String -> { | a } -> (PixiEff r Text)
newTextWithStyle text style = let t = runFn0 newText in do
  _ <- runFn2 setText text t
  _ <- runFn2 setTextStyle style t
  pure t

setMiddleAnchor :: forall r. Text -> PixiEff r Unit
setMiddleAnchor t = runFn3 setAnchor 0.5 0.5 t

--
-- Common properties
--
  
foreign import setPosition    :: forall t r. (IsDisObj t) => Fn2 Coords t (PixiEff r Unit)
foreign import setRotation    :: forall t r. (IsDisObj t) => Fn2 Number t (PixiEff r Unit)
foreign import setWidth       :: forall t r. (IsDisObj t) => Fn2 Number t (PixiEff r Unit)
foreign import setHeight      :: forall t r. (IsDisObj t) => Fn2 Number t (PixiEff r Unit)

foreign import getWidth       :: forall t r. (IsDisObj t) => t -> PixiEff r Number
foreign import getHeight      :: forall t r. (IsDisObj t) => t -> PixiEff r Number

setDim :: forall t r. (IsDisObj t) => Number -> Number -> t -> PixiEff r Unit
setDim w h obj = do
  _ <- runFn2 setWidth w obj
  _ <- runFn2 setHeight h obj
  pure unit

--
-- Buttons
--

foreign import setInteractive :: forall o r. (IsDisObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setButtonMode  :: forall o r. (IsDisObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setHitArea     :: forall s o r. IsShape s => IsDisObj o => Fn2 s o (PixiEff r Unit)

newButton :: forall s o r. IsShape s => IsDisObj o => s -> o -> PixiEff r Unit
newButton ha btn = do
  _   <- runFn2 setInteractive true btn
  _   <- runFn2 setButtonMode true btn
  _   <- runFn2 setHitArea ha btn
  pure unit

--
-- Containers
-- 

foreign import addToContainer :: forall o c r. IsDisObj o => IsCntr c => Fn2 o c (PixiEff r Unit)
foreign import removeAllFromContainer :: forall c r. (IsCntr c) => c -> (PixiEff r Unit)
foreign import _removeFromContainer :: forall o r. (IsDisObj o) => Fn1 o (PixiEff r Unit)

addToContainerAt :: forall o c r. IsDisObj o => IsCntr c => o -> Coords -> c -> (PixiEff r Unit)
addToContainerAt obj coords cnt = do
  _ <- runFn2 addToContainer obj cnt
  runFn2 setPosition coords obj

addToContainerAtMiddle :: forall c r. IsCntr c => Text -> Number -> Number -> c -> (PixiEff r Unit)
addToContainerAtMiddle txt w h cnt = do
  _ <- setMiddleAnchor txt
  _ <- addToContainerAt txt { x: w/2.0, y: h/2.0 } cnt
  pure unit

removeFromContainer :: forall o r. (IsDisObj o) => o -> PixiEff r Unit
removeFromContainer obj = runFn1 _removeFromContainer obj

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

onMouseDown :: forall a o r. (IsDisObj o) => (ChSend a) -> a -> o -> (PixiChEff r Unit)
onMouseDown ch msg obj = runFn2 _onMouseDown (send ch msg) obj

onMouseHover :: forall a o r. (IsDisObj o) => (ChSend a) -> a -> a -> o -> (PixiChEff r Unit)
onMouseHover ch msgIn msgOut obj = do
  _ <- runFn2 _onMouseOver (send ch msgIn) obj
  _ <- runFn2 _onMouseOut (send ch msgOut) obj
  pure unit

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

--
-- Any display object
--

data AnyDisObj = AnyDisObj (forall t. (forall o. IsDisObj o => o -> t) -> t)

anyDisObj :: forall o. IsDisObj o => o -> AnyDisObj
anyDisObj obj = AnyDisObj (\f -> f obj)

foldAnyDisObj :: forall t. AnyDisObj -> (forall o. IsDisObj o => o -> t) -> t
foldAnyDisObj (AnyDisObj anyObj) f = anyObj f
