module Pixi.Packer
 ( vpack
 , execVPacker
 , PackerState) where

import Control.Monad.Eff
import Data.Coords
import Data.Tuple
import Pixi
import Prelude
import Control.Monad.State.Class (put, get)
import Control.Monad.State.Trans (execStateT, StateT)
import Control.Monad.Trans (lift)

newtype AddFn = AddFn (forall r. AnyDisObj -> Coords -> PixiEff r Unit)
newtype PackerState = PackerState (Tuple Coords AddFn)
type Packer r a = StateT PackerState (Eff (pixi :: PIXI | r)) a

vpack :: forall r o. (IsDisObj o) => Number -> Number -> o -> Packer r Coords
vpack xOffset height obj = do
  (PackerState (Tuple currentCoords (AddFn doAdd))) :: PackerState <- get
  _ <- lift $ doAdd (anyDisObj obj) (addX xOffset currentCoords)
  let newCoords = addY height currentCoords
  _ <- put $ PackerState $ (Tuple newCoords (AddFn doAdd))
  return newCoords

execVPacker :: forall c r. (IsCntr c) => c -> Coords -> Packer r Unit -> PixiEff r Coords
execVPacker cntr start pckr = let
  addFn :: forall rr. AnyDisObj -> Coords -> PixiEff rr Unit
  addFn anyObj coords = foldAnyDisObj anyObj (\obj -> addToContainerAt obj coords cntr)
  startingState = PackerState $ (Tuple start (AddFn addFn))
  in stateCoords <$> execStateT pckr startingState

stateCoords :: PackerState -> Coords
stateCoords (PackerState (Tuple coords _)) = coords
