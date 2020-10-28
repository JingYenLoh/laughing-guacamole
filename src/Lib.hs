module Lib
  ( jsonNull,
    jsonArray,
    jsonBool,
    jsonNumber,
    jsonString,
    jsonValue,
    Value (..),
  )
where

import Text.Parsec

-- import Text.Parsec.Combinator (between)
-- import Text.Parsec.Language (emptyDef)

-- import Text.Parsec.Token (lexeme, makeTokenParser, whiteSpace)

type Parser = Parsec String ()

data Json = JsonElement Element

data Element = JsonValue Value

type Object = [(String, Value)]

data Value
  = JsonObject Object
  | JsonArray [Value]
  | JsonString String
  | JsonNumber Double
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

jsonNull :: Parser Value
jsonNull = JsonNull <$ string "null"

jsonObject :: Parser Value
jsonObject = undefined

jsonArray :: Parser Value
jsonArray = do
  char '['
  xs <- sepBy jsonValue (spaces *> char ',' <* spaces)
  char ']'
  return $ JsonArray xs

-- Doesn't handle escaped strings for now
jsonString :: Parser Value
jsonString = JsonString <$> (char '"' *> manyTill anyChar (char '"'))

-- see https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
-- parse positive int for now
jsonNumber :: Parser Value
jsonNumber = do
  n <- many1 digit
  return $ JsonNumber (read n)

jsonBool :: Parser Value
jsonBool =
  JsonBool True <$ string "true"
    <|> JsonBool False <$ string "false"

jsonValue :: Parser Value
jsonValue =
  jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonArray
    <|> jsonObject
