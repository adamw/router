module View.Fps (Fps, setup, update, render) where

import Prelude
import Pixi
import Data.Function

type Fps =
  { countInThisSecond :: Int
  , fpsInLastSecond :: Int
  , thisSecond :: Int
  , text :: Text
  }

setup :: forall t r. IsCntr t => t -> PixiEff r Fps
setup container = do
  text <- newText
  _ <- addToContainerAt text { x: 20.0, y: 20.0 } container
  return { countInThisSecond: 0, fpsInLastSecond: 0, thisSecond: 0, text: text }

update :: Int -> Fps -> Fps
update nowSecond fps = let
  fps' = if nowSecond /= fps.thisSecond
         then updateWithNewSecond nowSecond fps
         else fps
  in fps' { countInThisSecond = fps'.countInThisSecond + 1 }
  
updateWithNewSecond nowSecond fps = fps { fpsInLastSecond = fps.countInThisSecond
                                        , countInThisSecond = 0
                                        , thisSecond = nowSecond
                                        }

render :: forall r. Fps -> PixiEff r Unit
render fps = runFn2 setText ("FPS: " ++ (show fps.fpsInLastSecond)) fps.text
