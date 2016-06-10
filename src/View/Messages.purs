module View.Messages (Msgs, setup, update, render) where

import Data.Function
import Pixi
import Prelude

type Msgs =
  { msg :: String
  , text :: Text
  }

setup :: forall t r. IsCntr t => t -> PixiEff r Msgs
setup container = do
  text <- newText
  _ <- runFn2 addToContainer text container
  _ <- runFn2 setPosition { x: 20.0, y: 50.0 } text
  return { msg: "-", text: text }

update :: String -> Msgs -> Msgs
update msg msgs = msgs { msg = msg }

render :: forall r. Msgs -> PixiEff r Unit
render msgs = runFn2 setText msgs.msg msgs.text

