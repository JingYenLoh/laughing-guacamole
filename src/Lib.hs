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
jsonString = do
  _ <- char '"'
  s <- manyTill anyChar (char '"')
  return $ JsonString s

-- see https://www.schoolofhaskell.com/school/to-infinity-and-beyond/pick-of-the-week/parsing-floats-with-parsec
jsonNumber :: Parser Value
jsonNumber = undefined

jsonBool :: Parser Value
jsonBool =
  JsonBool True <$ string "true"
    <|> JsonBool False <$ string "false"

jsonValue :: Parser Value
jsonValue =
  -- jsonObject
  --   <|>
  jsonArray
    <|> jsonString
    <|> jsonNumber
    <|> jsonBool
    <|> jsonNull
