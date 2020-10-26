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
