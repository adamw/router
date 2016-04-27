module View.Messages where

import Data.Function
import Pixi
import Prelude

type Msgs =
  { msg :: String
  , text :: Text
  }

setupMsgs :: forall t r. IsContainer t => t -> PixiEff r Msgs
setupMsgs container = do
  text <- runFn0 newText
  _ <- runFn2 addToContainer text container
  _ <- runFn3 setPosition 20.0 50.0 text
  return { msg: "-", text: text }

updateMsgs :: String -> Msgs -> Msgs
updateMsgs msg msgs = msgs { msg = msg }

renderMsgs :: forall r. Msgs -> PixiEff r Unit
renderMsgs msgs = runFn2 setText msgs.msg msgs.text

