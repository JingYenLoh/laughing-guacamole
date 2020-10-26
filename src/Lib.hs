module Lib
  ( jsonNull,
    jsonBool,
    jsonValue,
    Value (..),
  )
where

import Text.Parsec
import Text.Parsec.Token (GenTokenParser (symbol))

type Parser = Parsec String ()

data Json = JsonElement Element

data Element = JsonValue Value

type Object = [(String, Value)]

data Value
  = JsonObject Object
  | JsonArray [Value]
  | JsonString String
  | JsonNumber Int
  | JsonBool Bool
  | JsonNull
  deriving (Eq, Show)

jsonNull :: Parser Value
jsonNull = JsonNull <$ string "null"

jsonObject :: Parser Value
jsonObject = undefined

jsonArray :: Parser Value
jsonArray = undefined

jsonString :: Parser Value
jsonString = undefined

jsonNumber = undefined

jsonBool :: Parser Value
jsonBool =
  JsonBool True <$ string "true"
    <|> JsonBool False <$ string "false"

jsonValue :: Parser Value
jsonValue =
  jsonObject
    <|> jsonArray
    <|> jsonString
    <|> jsonNumber
    <|> jsonBool
    <|> jsonNull
