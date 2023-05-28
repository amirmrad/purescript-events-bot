module Handlers
  ( MessageHandler
  , onMessage
  , onMessageCreate
  , onceReady
  , onQr
  , onChangeState
  , byChat
  , byFromMe
  ) where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Reader.Class (ask)
import Control.Monad.Trans.Class (lift)
import Control.Promise (toAff)
import Data.Either (either)
import Effect (Effect)
import Effect.Aff (launchAff_, makeAff, nonCanceler)
import Effect.Aff.Compat (EffectFn1, mkEffectFn1)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Foreign (readBoolean, readString, unsafeFromForeign, unsafeToForeign)
import Foreign.Index (readProp)
import Lazy.Joe (scoped)
import Types (Chat, Client, Message)
import WaBot (WaBot)

foreign import _onMessage :: Client -> EffectFn1 Message Unit -> Effect Unit
foreign import _onMessageCreate
  :: Client -> EffectFn1 Message Unit -> Effect Unit

foreign import _onQr :: Client -> EffectFn1 String Unit -> Effect Unit
foreign import _onceReady :: Client -> Effect Unit -> Effect Unit
foreign import _onChangeState :: Client -> EffectFn1 String Unit -> Effect Unit

type MessageHandler = Message -> WaBot Unit

onMessage :: MessageHandler -> WaBot Unit
onMessage h = do
  e <- ask
  liftEffect $
    e.client `_onMessage` mkEffectFn1 (launchAff_ <<< flip runReaderT e <<< h)

onMessageCreate :: MessageHandler -> WaBot Unit
onMessageCreate h = do
  e <- ask
  liftEffect $
    e.client `_onMessageCreate` mkEffectFn1
      (launchAff_ <<< flip runReaderT e <<< h)

onQr :: (String -> WaBot Unit) -> WaBot Unit
onQr h = do
  e <- ask
  liftEffect $
    e.client `_onQr` mkEffectFn1 (launchAff_ <<< flip runReaderT e <<< h)

onChangeState :: (String -> WaBot Unit) -> WaBot Unit
onChangeState h = do
  e <- ask
  liftEffect $
    e.client `_onChangeState` mkEffectFn1
      (launchAff_ <<< flip runReaderT e <<< h)

onceReady :: WaBot Unit
onceReady = do
  e <- ask
  lift $ makeAff $ \res -> nonCanceler <$ _onceReady e.client (res $ pure unit)

byChat :: Chat -> MessageHandler -> MessageHandler
byChat c h m = do
  let
    mustForward = do
      cidE <- (readProp "id" $ unsafeToForeign c)
        >>= readProp "_serialized"
        >>= readString
      mcidE <- readProp "getChat" (unsafeToForeign m)
        >>= (\f -> lift $ toAff $ scoped m (unsafeFromForeign f) unit)
        >>= unsafeToForeign >>> readProp "id"
        >>= readProp "_serialized"
        >>= readString
      pure $ cidE == mcidE
  res <- lift $ runExceptT mustForward
  either (liftEffect <<< logShow) (flip when (h m)) res

byFromMe :: MessageHandler -> MessageHandler
byFromMe h m = do
  let
    mustForward =
      (readProp "id" $ unsafeToForeign m) >>= readProp "fromMe" >>= readBoolean
  res <- lift $ runExceptT mustForward
  either (liftEffect <<< logShow) (flip when (h m)) res
