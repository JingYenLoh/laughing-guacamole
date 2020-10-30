module Lib
  ( jsonNull,
    jsonArray,
    jsonBool,
    jsonNumber,
    jsonObject,
    jsonString,
    jsonValue,
    JsonValue (..),
  )
where

import Control.Applicative (Applicative (liftA2))
import Text.Parsec
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as P

type Parser = Parsec String ()

data Json = JsonElement Element

data Element = JsonValue JsonValue

type Object = [(String, JsonValue)]

data JsonValue
  = JsonObject Object
  | JsonArray [JsonValue]
  | JsonString String
  | JsonNumber Double
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

lexer = P.makeTokenParser emptyDef

stringP = P.stringLiteral lexer

commaSepP = P.commaSep lexer

colonP = P.colon lexer

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> P.braces lexer (spaces *> members <* spaces)
  where
    members = P.commaSep lexer (spaces *> member <* spaces)
    member = liftA2 (,) (stringP <* spaces <* P.colon lexer) jsonValue

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> P.brackets lexer (sepBy jsonValue (spaces *> char ',' <* spaces))

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringP

-- see https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
-- parse positive int for now
jsonNumber :: Parser JsonValue
jsonNumber = JsonNumber . fromIntegral <$> P.integer lexer

jsonBool :: Parser JsonValue
jsonBool =
  JsonBool True <$ string "true"
    <|> JsonBool False <$ string "false"

jsonValue :: Parser JsonValue
jsonValue =
  jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject
