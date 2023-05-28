module Volley.Bot where

import Prelude

import Client (getChats)
import Control.Monad.Trans.Class (lift)
import Control.Promise (toAffE)
import Data.Array as Array
import Data.DateTime (adjust)
import Data.Either (Either(..), either, note)
import Data.Formatter.DateTime (format, parseFormatString)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (unwrap, wrap)
import Data.String as String
import Data.Time.Duration (Days(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect.Aff (error, throwError)
import Effect.Class (liftEffect)
import Effect.Class.Console (logShow)
import Effect.Now (nowDateTime)
import Lazy.Joe (effectful, scoped)
import Messenger.Run
  ( MESSENGER
  , MessengerLanguageF(..)
  , _messengerCapability
  , sendTextMessage
  )
import Parsing (runParser)
import Run (AFF, Run, case_, interpret, liftAff, on, peel, send)
import Run.Except (EXCEPT, rethrow, throw)
import Run.State (get, gets)
import Type.Prelude (Proxy(..))
import Type.Row (type (+))
import Types (Chat, Message)
import Unsafe.Coerce (unsafeCoerce)
import Volley.Parsing (volleyCmdP)
import Volley.Registry
  ( VolleyM
  , addPlayer
  , createMember
  , getMemberById
  , modifyMemberById
  , newGame
  , removePlayer
  )
import Volley.Types (Member, PlayerId, RegistryState, Volley, VolleyCmd(..))
import WaBot (WaBot)

-- tp :: forall r. VolleyM (EFFECT + MESSENGER + EXCEPT String + r) Unit
-- tp = do
--   addPlayer (Left "John")
--   now <- maybe (throw "bad date") pure $ toDateTime <$>
--     (instant $ wrap 1679085000000.0)
--   -- _ <- newGame { location: "IRI-School, NW6 5HE", time: now }
--   addPlayer (Left "Joe")
--   addPlayer (Left "Jack")
--   v <- maybe (throw "no game") pure =<< gets _.game
--   liftEffect <<< log =<< rethrow (formatVolley [] v)

volleyMsgHandler
  :: Message -> VolleyM (AFF + MESSENGER + EXCEPT String + ()) Unit
volleyMsgHandler msg' = do
  chat <- unsafeCoerce <$>
    (liftAff $ toAffE $ effectful (scoped msg msg.getChat) unit)
  sid <- senderId
  let
    handler = flip (either (throw <<< show)) vCmd
      ( case _ of
          AddPlayer -> do
            addPlayer (Right sid)
            pure unit
          AddGuest name -> do
            _ <- addPlayer (Left name)
            pure unit
          RegisterPlayer name -> do
            getMemberById sid >>=
              maybe
                (createMember sid name)
                (const $ modifyMemberById sid (_ { latestName = name }))
            game <- gets _.game
            case game of
              Nothing -> pure unit
              Just g -> when (Array.elem (Right sid) g.players) $
                publishGameState chat
          RemovePlayers indices -> do
            case indices of
              [] -> removePlayer (Right sid)
              _ -> removePlayer (Left indices)
            pure unit
          SetupGame location time -> when (isAdmin sid chat) do
            _ <- newGame { location, time: wrap time }
            pure unit
          NewGame -> when (isAdmin sid chat) do
            { heading: { location, time } } <-
              maybe (throw "no previous game for location copying") pure =<<
                gets _.game
            now <- liftAff $ liftEffect $ nowDateTime
            _ <-
              if now > unwrap time then do
                newDate <- maybe (throw "could not adjust date") pure $ adjust
                  (Days 7.0)
                  (unwrap time)
                newGame { location, time: wrap newDate }
              else newGame { location, time: time }
            pure unit
          Help -> publishHelp chat
      )
  prevState <- get
  res <- handler
  newState <- get
  when (prevState.game /= newState.game) $
    publishGameState chat
  pure res
  where
  msg = unsafeCoerce msg'
  vCmd = runParser msg.body volleyCmdP

  senderId = liftAff (toAffE $ effectful (scoped msg msg.getContact) unit) <#>
    (_.id >>> _.user)

  isAdmin :: String -> Chat -> Boolean
  isAdmin sid chat' = not chat.isGroup || fromMaybe false ma
    where
    chat = unsafeCoerce chat'
    ma =
      ( _.isAdmin <$> Array.find ((_ == sid) <<< _.user <<< _.id)
          chat.groupMetadata.participants
      )

  publishGameState
    :: forall r. Chat -> VolleyM (AFF + MESSENGER + EXCEPT String + r) Unit
  publishGameState chat = get >>=
    ( \s -> maybe (pure unit)
        ((sendTextMessage chat =<< _) <<< rethrow <<< formatVolley s.members)
        s.game
    )

  publishHelp
    :: forall r. Chat -> VolleyM (AFF + MESSENGER + EXCEPT String + r) Unit
  publishHelp chat = do
    sendTextMessage chat helpMsg
    publishGameState chat

helpMsg :: String
helpMsg =
  """  *Volleyball Bot*
_Commands:_
`/setup <timestamp> <loc>` - Setups a new game (*Admin only*)
`/new` - Create a new game list (*Admin only*)
`/register <name>` - register your nickname
`/add` - Add regisetered name to list. Does nothing if not registered.
`/add <guest_name>` - Add a name to list.
`/remove` - Remove registered  name from list. Does nothing if not registered.
`/remove <position>` - Remove a name from list.
`/help` - Show this message.

*TL;DR*

First:
`/register <name>` to register your  nickname
then:
`/add` to add name to list
finally:
`/remove` to remove name from list 

Any questions or suggestions feel free to contact garbageemailaddress@me.com
"""

mergeGameState :: RegistryState -> Array String -> RegistryState
mergeGameState rs names = fromMaybe rs $ do
  let
    game = map
      ( \name -> maybe (Left name) (Right <<< _.sid) $ Array.find
          (\m -> m.latestName == name)
          rs.members
      )
      names
  pure $ rs { game = rs.game <#> (_ { players = game }) }

formatVolley :: Array Member -> Volley -> Either String String
formatVolley ms { players, heading } = do
  strArr <- note "could not resolve player names"
    $ traverse (either pure resolvePlayerName) players
  let
    playersStr =
      String.joinWith "\n"
        $ map (\(Tuple i p) -> show i <> ". " <> p)
        $ Array.zip (Array.range 1 (Array.length strArr))
            strArr
  dateF <- parseFormatString "dddd, D of MMMM"
  timeF <- parseFormatString "HH:mm"
  let
    -- todo: add ordinal
    datetime = unwrap heading.time
    locStr = "_" <> heading.location <> "_"
    headingStr = format dateF datetime <> "\n" <> format timeF datetime <> "\n"
      <> locStr
  pure $ headingStr <> "\n\n" <> playersStr
  where
  resolvePlayerName :: PlayerId -> Maybe String
  resolvePlayerName pid = _.latestName <$> Array.find ((==) pid <<< _.sid) ms

messengerToStateAlg :: MessengerLanguageF ~> WaBot
messengerToStateAlg = case _ of
  SendMessage _chat _message next -> throwError (error "Not implemented") *>
    pure next
  SendTextMessage chat text next -> do
    let chat' = unsafeCoerce chat
    _ <- lift $ toAffE $ effectful (scoped chat' chat'.sendMessage) text
    pure next
  ReplyToMessage message text next -> do
    let message' = unsafeCoerce message
    _ <- lift $ toAffE $ effectful (scoped message' message'.reply) text
    pure next
  GetAllChats next -> getChats >>= pure <<< next
  GetChatMessages chat count next -> do
    let chat' = unsafeCoerce chat
    messages <- lift $ toAffE $ effectful (scoped chat' chat'.fetchMessages)
      count
    pure $ next messages

testMessengerToStateAlg :: forall r. MessengerLanguageF ~> Run (AFF + r)
testMessengerToStateAlg = case _ of
  SendMessage _chat _message next ->
    liftAff (throwError (error "Not implemented")) *> pure next
  SendTextMessage _chat text next -> do
    liftAff $ logShow text
    pure next
  ReplyToMessage message text next -> do
    let message' = unsafeCoerce message
    liftAff $ logShow $ message'.body <> " <-"
    liftAff $ logShow $ "  " <> text
    pure next
  GetAllChats next -> pure $ next []
  GetChatMessages chat count next -> do
    -- let chat' = unsafeCoerce chat
    pure $ next []

runMessengerTest
  :: forall r
   . Run (AFF + MESSENGER + r) ~> Run (AFF + r)
runMessengerTest = interpret
  (on _messengerCapability testMessengerToStateAlg send)

runAffWabot :: Run (AFF + ()) ~> WaBot
runAffWabot = interpret (case_ # on (Proxy :: Proxy "aff") lift)

runMessengerWabot :: Run (AFF + MESSENGER + ()) ~> WaBot
runMessengerWabot = loop
  where
  handleMessenger = on _messengerCapability Left Right
  handleAff = on (Proxy :: Proxy "aff") Left Right
  loop r = case peel r of
    Left a -> case handleMessenger a of
      Left mf -> messengerToStateAlg mf >>= loop
      Right a' -> case handleAff a' of
        Left af -> lift af >>= loop
        Right a'' -> case_ a'' >>= loop
    Right a ->
      pure a

initialState :: RegistryState
initialState = { game: Nothing, members: [] }
