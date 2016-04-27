module Pixi where

import Prelude
import Control.Monad.Eff (Eff())
import Data.Function

foreign import data PIXI :: !

foreign import data Renderer :: *
foreign import data Container :: *                    
foreign import data Text :: *
foreign import data Graphics :: *
                    
foreign import data Circle :: *                    

type PixiEff r t = Eff (pixi :: PIXI | r) t

foreign import newRenderer :: forall r. Fn2 Int Int (PixiEff r Renderer)
foreign import newContainer :: forall r. Fn0 (PixiEff r Container)
foreign import newText :: forall r. Fn0 (PixiEff r Text)
foreign import newGraphics :: forall r. Fn0 (PixiEff r Graphics)

foreign import newCircle :: forall r. Fn3 Int Int Int (PixiEff r Graphics)

foreign import appendRendererToBody :: forall r. Renderer -> PixiEff r Unit
foreign import renderContainer :: forall t r. (IsContainer t) => Fn2 t Renderer (PixiEff r Unit)

foreign import setBackgroundColor :: forall r. Fn2 Int Renderer (PixiEff r Unit)
foreign import setText :: forall r. Fn2 String Text (PixiEff r Unit)

foreign import setPosition :: forall t r. (Positionable t) => Fn3 Int Int t (PixiEff r Unit)

foreign import setInteractive :: forall o r. (IsDisplayObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setButtonMode :: forall o r. (IsDisplayObj o) => Fn2 Boolean o (PixiEff r Unit)
foreign import setHitArea :: forall s o r. (IsShape s, IsDisplayObj o) => Fn2 s o (PixiEff r Unit)

foreign import addToContainer :: forall c t r. (IsDisplayObj c, IsContainer t) => Fn2 c t (PixiEff r Unit)

-- TODO: Color
foreign import beginFill  :: forall r. Fn3 Int Number     Graphics (PixiEff r Graphics)
foreign import lineStyle  :: forall r. Fn4 Int Int Number Graphics (PixiEff r Graphics)
foreign import drawCircle :: forall r. Fn4 Int Int Int    Graphics (PixiEff r Graphics)
foreign import endFill    :: forall r. Fn1                Graphics (PixiEff r Graphics)

class IsDisplayObj o
instance containerIsDisplayObject :: IsDisplayObj Container
instance textIsDisplayObject :: IsDisplayObj Text
instance graphicsIsDisplayObject :: IsDisplayObj Graphics

class IsContainer t
instance containerIsContainer :: IsContainer Container

class Positionable t
instance textIsPositionable :: Positionable Text

class IsShape c
instance circleIsShape :: IsShape Circle
