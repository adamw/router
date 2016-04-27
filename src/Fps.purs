module Fps (Fps, setupFps, updateFps, renderFps) where

import Prelude
import Pixi
import Data.Function

type Fps =
  { countInThisSecond :: Int
  , fpsInLastSecond :: Int
  , thisSecond :: Int
  , text :: Text
  }

setupFps :: forall t r. IsContainer t => t -> PixiEff r Fps
setupFps container = do
  text <- runFn0 newText
  _ <- runFn2 addToContainer text container
  _ <- runFn3 setPosition 20 20 text
  return { countInThisSecond: 0, fpsInLastSecond: 0, thisSecond: 0, text: text }

updateFps :: Int -> Fps -> Fps
updateFps nowSecond fps = let
  fps' = if nowSecond /= fps.thisSecond
         then updateWithNewSecond nowSecond fps
         else fps
  in fps' { countInThisSecond = fps'.countInThisSecond + 1 }
  
updateWithNewSecond nowSecond fps = fps { fpsInLastSecond = fps.countInThisSecond
                                        , countInThisSecond = 0
                                        , thisSecond = nowSecond
                                        }

renderFps :: forall r. Fps -> PixiEff r Unit
renderFps fps = runFn2 setText ("FPS: " ++ (show fps.fpsInLastSecond)) fps.text
