module Volley.Types where

import Prelude

import Data.Argonaut (class DecodeJson, JsonDecodeError(..), stringify)
import Data.Argonaut.Decode.Class (decodeJson)
import Data.Argonaut.Encode.Class (class EncodeJson, encodeJson)
import Data.DateTime (DateTime)
import Data.DateTime.Instant (fromDateTime, instant, toDateTime, unInstant)
import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Show.Generic (genericShow)

newtype GameTime = GameTime DateTime

derive instance Newtype GameTime _
derive instance Eq GameTime

instance Show GameTime where
  show = show <<< unwrap

instance EncodeJson GameTime where
  encodeJson = encodeJson <<< unwrap <<< unInstant <<< fromDateTime <<< unwrap

instance DecodeJson GameTime where
  decodeJson t = do
    n <- decodeJson t
    i <-
      note
        (TypeMismatch $ "could not convert '" <> stringify t <> "' to instant")
        $ instant
        $ wrap n
    pure $ wrap $ toDateTime i

type PlayerId = String
type GuestName = String

type Member =
  { sid :: PlayerId
  , latestName :: String
  , history ::
      Array Int -- 0 for non attendance, 1 for attendance -1 for removal
  }

type Heading =
  { location :: String
  , time :: GameTime
  }

type Volley =
  { heading :: Heading
  , players :: Array (Either GuestName PlayerId)
  }

type RegistryState =
  { game :: Maybe Volley
  , members :: Array Member
  }

data VolleyCmd
  = AddPlayer
  | AddGuest String
  | RegisterPlayer String
  | RemovePlayers (Array Int)
  | SetupGame String DateTime
  | NewGame
  | Help

derive instance Generic VolleyCmd _

instance Show VolleyCmd where
  show = genericShow
