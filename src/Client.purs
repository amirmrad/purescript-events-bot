module Client
  ( createClient
  , initializeClient
  , getChatBySerializedId
  , getState
  , getChats
  ) where

import Prelude

import Control.Monad.Reader.Trans (ask, asks)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise, toAffE)
import Data.Maybe (Maybe)
import Effect (Effect)
import FfiHelpers (MaybeFfiHelper, maybeFfiHelper)
import Types (Chat, Client)
import WaBot (WaBot)

foreign import _initializeClient :: Client -> Effect (Promise Unit)
foreign import _createClient :: Effect Client
foreign import _getChats :: Client -> Effect (Promise (Array Chat))
foreign import _getChatById :: Client -> String -> Effect (Promise Chat)
foreign import _getState
  :: MaybeFfiHelper -> Client -> Effect (Promise (Maybe String))

createClient :: Effect Client
createClient = _createClient

initializeClient :: WaBot Unit
initializeClient = do
  { client } <- ask
  lift $ toAffE $ _initializeClient client

getState :: WaBot (Maybe String)
getState = do
  { client } <- ask
  lift $ toAffE $ _getState maybeFfiHelper client

getChatBySerializedId :: String -> WaBot Chat
getChatBySerializedId sid = lift <<< toAffE <<< flip _getChatById sid =<< asks
  _.client

getChats :: WaBot (Array Chat)
getChats = lift <<< toAffE <<< _getChats =<< asks _.client
