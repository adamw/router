module ChSend(ChSend, chSend, send) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Functor.Contravariant (class Contravariant)
import Signal.Channel (CHANNEL, Channel, send) as O

newtype ChSend a = ChSend forall r. a -> Eff (channel :: O.CHANNEL | r) Unit

chSend :: forall a. O.Channel a -> ChSend a
chSend ch = ChSend (\v -> O.send ch v)

send :: forall a r. ChSend a -> a -> Eff (channel :: O.CHANNEL | r) Unit
send (ChSend s) = s

instance chSendContravariant :: Contravariant ChSend where
  cmap f (ChSend s) = ChSend (s <<< f)
