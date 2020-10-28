import Lib
import Test.Hspec
import Test.Hspec.Parsec
import Test.Tasty
import Test.Tasty.Hspec
import Text.Parsec

main = do
  spec <- testSpec "Spec tests" specTests
  defaultMain $ testGroup "tests" [spec]

specTests :: Spec
specTests = do
  pJsonBoolSpec
  pJsonNullSpec
  pJsonNumberSpec
  pJsonStringSpec
  pJsonArraySpec

pJsonObjectSpec :: Spec
pJsonObjectSpec = describe "pJsonObject" $ do
  it "parses empty object" $ do
    parse jsonObject "" "{}" `shouldParse` (JsonObject [])

pJsonArraySpec :: Spec
pJsonArraySpec = describe "pJsonArray" $ do
  it "parses empty array" $ do
    parse jsonArray "" "[]" `shouldParse` (JsonArray [])
  it "parses array with one element" $ do
    parse jsonArray "" "[1]" `shouldParse` (JsonArray [JsonNumber 1.0])
  it "does not parse unpaired (left) brackets" $ do
    parse jsonArray "" `shouldFailOn` "[[]"
  it "does not parse unpaired (right) brackets" $ do
    parse jsonArray "" `shouldFailOn` "[]]"

pJsonStringSpec :: Spec
pJsonStringSpec = describe "pJsonString" $ do
  it "parses empty string" $ do
    parse jsonString "" "\"\"" `shouldParse` (JsonString "")
  it "parses non-empty string" $ do
    parse jsonString "" "\"cs2104\"" `shouldParse` (JsonString "cs2104")

pJsonNumberSpec :: Spec
pJsonNumberSpec = describe "pJsonNumber" $ do
  it "parses 0" $ do
    parse jsonNumber "" "0" `shouldParse` (JsonNumber 0)
  it "parses -1" $ do
    parse jsonNumber "" "-1" `shouldParse` (JsonNumber (-1))
  it "parses 0.0" $ do
    parse jsonNumber "" "0.0" `shouldParse` (JsonNumber 0)
  it "parses 0.2" $ do
    parse jsonNumber "" "0.2" `shouldParse` (JsonNumber 0.2)

pJsonBoolSpec :: Spec
pJsonBoolSpec = describe "pJsonBool" $ do
  it "parses true" $ do
    parse jsonBool "" "true" `shouldParse` (JsonBool True)
  it "parses false" $ do
    parse jsonBool "" "false" `shouldParse` (JsonBool False)
  it "does not parse frue" $ do
    parse jsonBool "" `shouldFailOn` "frue"
  it "does not parse talse" $ do
    parse jsonBool "" `shouldFailOn` "talse"

pJsonNullSpec :: Spec
pJsonNullSpec = describe "pJsonNull" $ do
  it "parses null" $ do
    parse jsonNull "" "null" `shouldParse` JsonNull

  it "does not parse nul" $ do
    parse jsonNull "" `shouldFailOn` "nul"
