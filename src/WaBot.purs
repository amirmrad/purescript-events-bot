module WaBot where

import Control.Monad.Reader (ReaderT)
import Effect.Aff (Aff)
import Types (Client)

type WaBotHandle =
  { client :: Client
  }

type WaBot = ReaderT WaBotHandle Aff
