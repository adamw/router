module View.Messages (MsgsState, MsgsViewState, setup, update, draw) where

import Data.Function
import Pixi
import Prelude
import Data.Tuple (Tuple(Tuple))

type MsgsState = { msg :: String }

type MsgsViewState = { text :: Text }

setup :: forall t r. IsCntr t => t -> PixiEff r (Tuple MsgsState MsgsViewState)
setup container = do
  text <- newText
  _ <- runFn2 addToContainer text container
  _ <- runFn2 setPosition { x: 20.0, y: 50.0 } text
  pure $ Tuple { msg: "-" } { text: text }

update :: String -> MsgsState -> MsgsState
update msg msgs = msgs { msg = msg }

draw :: forall r. MsgsState -> MsgsViewState -> PixiEff r Unit
draw msgs msgsView = runFn2 setText msgs.msg msgsView.text

