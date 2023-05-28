module Messenger.Run where

import Prelude

import Run (Run(..), lift, send)
import Type.Prelude (Proxy(Proxy))
import Type.Row (type (+))
import Types (Chat, Message)

type Count = Int

data MessengerLanguageF a
  = SendMessage Chat Message a
  | SendTextMessage Chat String a
  | ReplyToMessage Message String a
  | GetAllChats (Array Chat -> a)
  | GetChatMessages Chat Count (Array Message -> a)

derive instance Functor MessengerLanguageF

_messengerCapability :: Proxy "messenger"
_messengerCapability = Proxy

type MESSENGER r = (messenger :: MessengerLanguageF | r)

sendMessage :: forall r. Chat -> Message -> Run (MESSENGER + r) Unit
sendMessage chat message = lift _messengerCapability $
  (SendMessage chat message unit)

sendTextMessage :: forall r. Chat -> String -> Run (MESSENGER + r) Unit
sendTextMessage chat message = lift _messengerCapability $
  (SendTextMessage chat message unit)

replyToMessage :: forall r. Message -> String -> Run (MESSENGER + r) Unit
replyToMessage message reply = lift _messengerCapability $
  (ReplyToMessage message reply unit)

getAllChats :: forall r. Run (MESSENGER + r) (Array Chat)
getAllChats = lift _messengerCapability $ (GetAllChats identity)
