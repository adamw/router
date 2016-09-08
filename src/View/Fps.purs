module View.Fps (FpsState, FpsViewState, setup, update, draw) where

import Prelude
import Pixi
import Data.Function.Uncurried(runFn0, runFn2)
import Data.Tuple (Tuple(Tuple))

type FpsState =
  { countInThisSecond :: Int
  , fpsInLastSecond :: Int
  , thisSecond :: Int
  }

type FpsViewState = { text :: Text }

setup :: forall t r. IsCntr t => t -> PixiEff r (Tuple FpsState FpsViewState)
setup container = let text = runFn0 newText in do
  _ <- addToContainerAt text { x: 20.0, y: 60.0 } container
  pure $ Tuple { countInThisSecond: 0, fpsInLastSecond: 0, thisSecond: 0 } { text: text }

update :: Int -> FpsState -> FpsState
update nowSecond fps = let
  fps' = if nowSecond /= fps.thisSecond
         then updateWithNewSecond nowSecond fps
         else fps
  in fps' { countInThisSecond = fps'.countInThisSecond + 1 }
  
updateWithNewSecond nowSecond fps = fps { fpsInLastSecond = fps.countInThisSecond
                                        , countInThisSecond = 0
                                        , thisSecond = nowSecond
                                        }

draw :: forall r. FpsState -> FpsViewState -> PixiEff r Unit
draw fps fpsView = runFn2 setText ("FPS: " <> (show fps.fpsInLastSecond)) fpsView.text
