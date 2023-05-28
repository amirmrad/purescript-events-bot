module Playground where

import Prelude

import Control.Alt (class Alt)
import Control.Monad.Free (Free, liftF, runFreeM)
import Control.Monad.Reader (ReaderT, runReaderT)
import Control.Monad.Reader.Trans (ask)
import Control.Monad.Rec.Class (class MonadRec, Step(..))
import Control.Plus (class Plus)
import Data.Array (head)
import Data.Either (Either(..))
import Data.Functor (voidLeft, voidRight)
import Data.Functor.Variant (VariantF, case_, on)
import Data.Maybe (maybe)
import Data.Newtype (class Newtype)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (log, logShow)
import Run (Run, Step(..), extract, interpret, lift, peel, run, runPure)
import Type.Prelude (Proxy(Proxy))
import Type.Row (type (+))

data BaseCapabilityF a
  = SendTextMessage String a
  | GetLastMessages (Array String -> a)

derive instance Functor BaseCapabilityF

type BASE r = (base :: BaseCapabilityF | r)

_baseCapability :: Proxy "base"
_baseCapability = Proxy

sendTextMessage :: forall r. String -> Run (BASE + r) Unit
sendTextMessage s = lift _baseCapability $ SendTextMessage s unit

getLastMessages :: forall r. Run (BASE + r) (Array String)
getLastMessages = lift _baseCapability $ GetLastMessages identity

program :: forall r. Run (BASE + r) Unit
program = do
  msgs <- getLastMessages
  msgs <- getLastMessages
  msgs <- getLastMessages
  msgs <- getLastMessages
  msgs <- getLastMessages
  msgs <- getLastMessages
  maybe
    (sendTextMessage "No messages")
    (\m -> sendTextMessage $ "Last message: " <> m)
    (head msgs)

baseAffAlgebra :: BaseCapabilityF ~> Aff
baseAffAlgebra = case _ of
  SendTextMessage s next -> liftEffect (log s) *> pure next
  GetLastMessages next -> pure $ next [ "Hello", "World" ]

baseEffAlgebra :: BaseCapabilityF ~> Effect
baseEffAlgebra = case _ of
  SendTextMessage s next -> (log s) *> pure next
  GetLastMessages next -> pure $ next [ "Hello", "World" ]

baseDebugAlgebra'
  :: forall r
   . (VariantF r String -> String)
  -> VariantF (BASE + r) String
  -> String
baseDebugAlgebra' = on _baseCapability $ case _ of
  SendTextMessage s next -> "SendTextMessage " <> s <> "\n" <> next
  GetLastMessages next -> "GetLastMessages" <> "\n" <> next [ "Hello", "World" ]

baseDebugAlgebra
  :: forall r
   . BaseCapabilityF (Run (BASE + r) String)
  -> Step
       (Run (BASE + r) String)
       (VariantF r (Run (BASE + r) String))
baseDebugAlgebra = case _ of
  SendTextMessage s next -> Loop $
    (next <#> \next' -> next' <> "SendTextMessage " <> s)
  GetLastMessages next -> Loop $
    ( next [ "Hello", "Other stuff" ] <#> \next' -> next' <> "GetLastMessages"
        <> "\n"
    )

runBaseAff :: forall a. Run (BASE + ()) a -> Aff a
runBaseAff = interpret (case_ # on _baseCapability baseAffAlgebra)

runBaseEff :: forall a. Run (BASE + ()) a -> Effect a
runBaseEff = interpret (case_ # on _baseCapability baseEffAlgebra)

runBaseDebug :: forall a. Run (BASE + ()) a -> String
runBaseDebug = extract <<< runPure (case_ # on _baseCapability baseDebugAlgebra)
  <<< voidRight "V"

runBaseDebug' :: forall a. Run (BASE + ()) a -> String
runBaseDebug' = myIter (baseDebugAlgebra' case_)

myIter
  :: forall a
   . (VariantF (BASE + ()) String -> String)
  -> Run (BASE + ()) a
  -> String
myIter k a = loop $ ("V" <$ a)
  where
  loop r' = case peel r' of
    Left ra -> k (loop <$> ra)
    Right a' -> a'

--   (interpret (pure <<< baseDebugAlgebra (case_ :: VariantF () a -> String) :: Run (BASE + ()) String -> Run () String))
-- interpret (case_ # on _baseCapability baseDebugAlgebra)
--(on _baseCapability baseAffAlgebra)

-- type BaseCapability c = 
--   { sendTextMessage :: String -> Aff Unit | c}
-- 
-- type Cmd (c :: Row Type) a = ReaderT (BaseCapability c) Aff a
-- 
-- 
-- runCmd :: forall (c :: Row Type) (a :: Type). Cmd c a -> BaseCapability c -> Aff a
-- runCmd = runReaderT
-- 
-- cap :: forall (c :: Row Type) (a :: Type). (BaseCapability c -> Aff a) -> Cmd c a
-- cap h = ask >>= lift <<< h

-- newtype Cmd (c :: Row Type) a = Cmd (ReaderT (BaseCapability c) Aff a)
-- 
-- derive newtype instance Functor (Cmd r)
-- derive newtype instance Apply (Cmd r)
-- derive newtype instance Applicative (Cmd r)
-- derive newtype instance Alt (Cmd r)
-- derive newtype instance Plus (Cmd r)
-- derive newtype instance Bind (Cmd r)
-- derive newtype instance Monad (Cmd r)
-- derive newtype instance MonadEffect (Cmd r)
-- derive newtype instance MonadAff (Cmd r)
-- derive newtype instance Semigroup a => Semigroup ((Cmd r) a)
-- derive newtype instance Monoid a => Monoid ((Cmd r) a)
-- derive newtype instance MonadRec (Cmd r)
-- 
-- 
-- sendTextMessage :: forall (r :: Row Type). String -> Cmd r Unit
-- sendTextMessage s = Cmd $ do
--   handle <- ask
--   lift $ handle.sendTextMessage s

-- Free stuff

-- type SomeHandle = 
--   { sendMessage :: String -> Aff Unit
--   }
-- 
-- data CommandF a = Action a
-- 
-- instance Functor CommandF where
--   map f (Action a) = Action (f a)
-- 
-- type Command = Free CommandF
-- 
-- action :: Command Unit
-- action = liftF (Action unit)
-- 
-- runCommand :: forall a. Command a -> Aff a
-- runCommand = runFreeM go
--   where go (Action a) = logShow "Action" *> pure a
-- 
-- cmd = do
--   lift $ logShow "Going to run command"
--   action
--   logShow "Command finished"

