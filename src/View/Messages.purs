module View.Messages (Msgs, setup, update, render) where

import Data.Function
import Pixi
import Prelude

type Msgs =
  { msg :: String
  , text :: Text
  }

setup :: forall t r. IsContainer t => t -> PixiEff r Msgs
setup container = do
  text <- runFn0 newText
  _ <- runFn2 addToContainer text container
  _ <- runFn3 setPosition 20.0 50.0 text
  return { msg: "-", text: text }

update :: String -> Msgs -> Msgs
update msg msgs = msgs { msg = msg }

render :: forall r. Msgs -> PixiEff r Unit
render msgs = runFn2 setText msgs.msg msgs.text

