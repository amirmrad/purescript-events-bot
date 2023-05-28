module Helpers where

import Prelude

import Client (getState)
import Control.Monad.Trans.Class (lift)
import Control.Promise (toAffE)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (liftEffect)
import Handlers (onQr, onceReady)
import Lazy.Joe (effectful, scoped)
import Types (Chat, Message)
import Unsafe.Coerce (unsafeCoerce)
import WaBot (WaBot)

foreign import _showQr :: String -> Effect Unit

defaultOnQr :: WaBot Unit
defaultOnQr = onQr (liftEffect <<< _showQr)

getMessageChat :: Message -> WaBot Chat
getMessageChat m = lift $ toAffE $ effectful
  (scoped (unsafeCoerce m) (unsafeCoerce m).getChat)
  unit

waitTillReady :: WaBot Unit
waitTillReady = getState >>= maybe onceReady (const $ pure unit)
