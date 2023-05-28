module Volley.Parsing where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.DateTime.Instant (instant, toDateTime)
import Data.Int (fromString) as Int
import Data.List (manyRec)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (wrap)
import Data.Number (fromString) as Number
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested (type (/\), (/\))
import Debug (trace)
import Parsing (Parser, fail, runParser)
import Parsing.Combinators (sepBy, try)
import Parsing.Combinators.Array (many, many1, manyTill_)
import Parsing.String (anyChar, char, rest, satisfy, string)
import Parsing.String.Basic (alphaNum, digit, whiteSpace)
import Volley.Types (VolleyCmd(..))

type Cmd = String
type Arg = String

trimmedP :: forall (a :: Type). Parser String a -> Parser String a
trimmedP p = many (char ' ') *> p <* many (char ' ')

notNewlineP :: Parser String Char
notNewlineP = satisfy \c -> c /= '\n'

lineP :: Parser String String
lineP = fromCharArray <$> many notNewlineP <* char '\n'

wordP :: Parser String String
wordP = fromCharArray <<< Array.fromFoldable <$> many1
  (satisfy \c -> c /= ' ' && c /= '\n')

cmdP :: Parser String (Cmd /\ Array Arg)
cmdP = Tuple <$> (char '/' *> wordP) <*> many (trimmedP wordP)

--  todo: use optparse
volleyCmdP :: Parser String VolleyCmd
volleyCmdP = cmdP >>= case _ of
  "add" /\ args -> case args of
    [] -> pure AddPlayer
    xs -> pure (AddGuest $ String.joinWith " " xs)
  "remove" /\ args -> case args of
    [] -> pure $ RemovePlayers []
    xs -> maybe (fail "invalid index") (pure <<< RemovePlayers) $ traverse
      Int.fromString
      xs
  "register" /\ args -> case args of
    [] -> fail "missing name"
    xs -> pure (RegisterPlayer $ String.joinWith " " xs)
  "setup" /\ args -> case args of
    [] -> fail "missing arguments"
    xs -> case Array.uncons xs of
      Just { head, tail } -> ado
        date <- maybe (fail "invalid epoch date") pure
          $ map toDateTime <<< instant <<< wrap <=< Number.fromString
          $ head
        loc <- pure $ String.joinWith " " tail
        in SetupGame loc date
      _ -> fail "not enough arguments"
  "new" /\ args -> case args of
    [] -> pure NewGame
    _ -> fail "new does not take arguments"
  "help" /\ args -> case args of
    _ -> pure Help
  _ -> fail "invalid command"

t2 :: Maybe Number
t2 = Number.fromString "1678649487000"

cmdT1 = runParser t1 volleyCmdP
  where
  t1 = "/setup 1678649487000 aosethuoashu"

-- registryCmdP :: Parser String (Cmd /\ Array Arg)
-- registryCmdP = Tuple <$> (char '/' *> anyChar <*> 
--   ( ado
--       _ <- string "add"
--       name <- trimmedP (fromCharArray <$> many notNewline)
--       in Add name
--   )
--   ( do
--       _ <- string "remove"
--       index <- trimmedP number
--       maybe (fail "invalid index") (pure <<< Remove) $ fromNumber index
--   )

teststr =
  """asntohustaoh
saotehsa
asonethu

1. Amir
2. Ali
3. Gholi
4. Reza
5. Saeed
6. Sina

aoenuhoa
  aoteh aou oauh9924p
r

7. gafooooooooooooe


"""

teststr2 =
  """
1.Amir
2.Ali
3. Gholi 
  asothueantohut
4. Reza
"""

t = runParser teststr volleyP

tt = runParser teststr2 xp -- (sepBy nameEntryP (try $ char '\n') <* rest)

k = runParser "aoeu,aoeu,oeu,haoeua" $ sepBy (many1 alphaNum) (char ',')

xp = manyRec $ try (char '\n') *> (nameEntryP <|> (Nothing <$ notNewlineP))

volleyP :: Parser String (Array String)
volleyP = ado
  (_ /\ h) <- manyTill_ anyChar (try $ char '\n' *> nameEntryP)
  -- v this is a hacky way to prevent failure when sepBy parser fails
  t <- Array.fromFoldable <$> sepBy
    (nameEntryP <|> (Nothing <$ many notNewlineP))
    (char '\n')
  _ <- rest
  in Array.catMaybes (Array.cons h t)

nameEntryP :: Parser String (Maybe String)
nameEntryP =
  many1 digit *> separator *> trimmedP
    ( fromCharArray
        <$> many notNewlineP
    ) <#>
    ( case _ of
        "" -> Nothing
        s -> Just s
    )
  where
  separator :: Parser String Char
  separator = char ')' <|> char '.'
