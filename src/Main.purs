module Main where

import Prelude

import ActivityBot (testActivityBot)
import Effect (Effect)
import Effect.Aff (launchAff)
import Effect.Console (log)

main :: Effect Unit
main = do
  log "Purescript whatsapp bot"
  _ <- launchAff testActivityBot
  pure unit

-- _on client "qr" (mkEffectFn1 showQr)
-- client `_on` "ready" $ mkEffectFn1 $ const $ setupHandlers client
