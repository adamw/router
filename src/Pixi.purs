module Pixi where

import Prelude
import Control.Monad.Eff (Eff())
import Data.Function

foreign import data PIXI :: !

foreign import data Renderer :: *
foreign import data Container :: *                    
foreign import data Text :: *

type PixiEff r t = Eff (pixi :: PIXI | r) t

foreign import newRenderer :: forall r. Fn2 Int Int (PixiEff r Renderer)
foreign import newContainer :: forall r. Fn0 (PixiEff r Container)
foreign import newText :: forall r. Fn0 (PixiEff r Text)

foreign import appendRendererToBody :: forall r. Renderer -> PixiEff r Unit
foreign import render :: forall t r. (IsContainer t) => Fn2 t Renderer (PixiEff r Unit)

foreign import setBackgroundColor :: forall r. Fn2 Int Renderer (PixiEff r Unit)
foreign import setText :: forall r. Fn2 String Text (PixiEff r Unit)

foreign import setPosition :: forall t r. (Positionable t) => Fn3 Int Int t (PixiEff r Unit)

foreign import addToContainer :: forall c t r. (IsDisplayObject c, IsContainer t) => Fn2 c t (PixiEff r Unit)

class IsDisplayObject c
instance containerIsDisplayObject :: IsDisplayObject Container
instance textIsDisplayObject :: IsDisplayObject Text

class IsContainer t
instance containerIsContainer :: IsContainer Container

class Positionable t
instance textIsPositionable :: Positionable Text
