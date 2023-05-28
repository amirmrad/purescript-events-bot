module ActivityBot where

import Prelude

import Client (createClient, getChatBySerializedId, initializeClient)
import Control.Monad.Error.Class (catchError, throwError)
import Control.Monad.Reader.Trans (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (decodeJson, stringify)
import Data.Argonaut.Encode.Class (encodeJson)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (either, isLeft)
import Data.Traversable (for_)
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, error)
import Effect.Class (liftEffect)
import Effect.Class.Console (log, logShow)
import Handlers
  ( MessageHandler
  , byChat
  , byFromMe
  , onChangeState
  , onMessage
  , onMessageCreate
  )
import Helpers (defaultOnQr, waitTillReady)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Parsing (runParser)
import Run.Except (runExcept)
import Run.State (runState)
import Unsafe.Coerce (unsafeCoerce)
import Volley.Bot
  ( initialState
  , mergeGameState
  , runMessengerWabot
  , volleyMsgHandler
  )
import Volley.Parsing (volleyP)
import Volley.Registry (runRegistry)
import Volley.Types (RegistryState)
import WaBot (WaBot, WaBotHandle)

testActivityBot :: Aff WaBotHandle
testActivityBot = do
  client <- liftEffect createClient
  let botHandle = { client }
  flip runReaderT botHandle do
    defaultOnQr
    initializeClient
    waitTillReady
    liftEffect $ log "Bot is ready"
    setupHandlers
  pure botHandle

testMsgHandler :: String -> MessageHandler
testMsgHandler prefix m = do
  let filename = prefix <> "-state.json"
  (currentState :: RegistryState) <- flip catchError
    (\e -> logShow e *> pure initialState)
    do
      lastStateJsonFile <- lift $ jsonParser <$> readTextFile UTF8
        filename
      lastStateJson <- either (throwError <<< error) pure lastStateJsonFile
      either (throwError <<< error <<< show) pure $ decodeJson lastStateJson

  baseState <-
    either (const $ pure currentState)
      (pure <<< mergeGameState currentState)
      $ runParser (unsafeCoerce m).body volleyP

  (newState /\ resE) <- volleyMsgHandler m # runRegistry # runExcept
    # runState baseState
    # runMessengerWabot

  when (isLeft resE) do
    log $ "Error: " <> show resE
    logShow currentState
  logShow newState

  lift $ writeTextFile UTF8 filename $ stringify $ encodeJson
    newState

  pure unit

setupHandlers :: WaBot Unit
setupHandlers = do
  log "Setting up handlers"
  vChat <- getChatBySerializedId volleyballChat
  tChat <- getChatBySerializedId liveTenvChat
  tChat <- getChatBySerializedId localTenvChat

  let
    commonMessageHandler =
      for_
        [ byChat tChat (testMsgHandler "tenv")
        , byChat vChat (testMsgHandler "volley")
        ] <<< (#)

  onMessage commonMessageHandler
  onMessageCreate $ byFromMe commonMessageHandler
  onChangeState logShow

t1 :: String
t1 =
  "{{Day}} {{Time}} {{Desc}}"
