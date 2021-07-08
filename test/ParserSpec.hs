module ParserSpec where
  import Test.Hspec

  import Text.Parsec (ParseError, runParser)
  import Text.Parsec.String (Parser)

  import qualified Care.Parser as Parser
  import Care.Value

  parse :: Parser a -> String -> Either ParseError a
  parse p = runParser p () ""

  spec :: Spec
  spec = do
    describe "number" testNumber

  testNumber :: Spec
  testNumber = do
    it "parses a positive integer" $ do
      parse Parser.number "12" `shouldBe` (Right (careInteger 12))
    it "parses a positive integer prefixed with a plus" $ do
      parse Parser.number "+12" `shouldBe` (Right (careInteger 12))
    it "parses a negative integer" $ do
      parse Parser.number "-12" `shouldBe` (Right $ careInteger (-12))
    it "parses a positive float" $ do
      parse Parser.number "12.0" `shouldBe` (Right (careFloat 12.0))
    it "parses a positive float prefixed with a plus" $ do
      parse Parser.number "+12.0" `shouldBe` (Right (careFloat 12.0))
    it "parses a negative float" $ do
      parse Parser.number "-12.0" `shouldBe` (Right $ careFloat (-12.0))
