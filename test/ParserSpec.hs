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
    describe "decimal number" testDecimalNumber
    describe "hexadecimal number" testHexadecimalNumber
    describe "string literal" testString

  testDecimalNumber :: Spec
  testDecimalNumber = do
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

  testHexadecimalNumber :: Spec
  testHexadecimalNumber = do
    it "parses a hexadecimal positive number" $ do
      parse Parser.number "0x10" `shouldBe` (Right (careInteger 16))
      parse Parser.number "0x5a" `shouldBe` (Right (careInteger 90))
    it "parses a hexadecimal negative number" $ do
      parse Parser.number "-0x10" `shouldBe` (Right (careInteger (-16)))
      parse Parser.number "-0x5a" `shouldBe` (Right (careInteger (-90)))

  testString :: Spec
  testString = do
    it "parses an empty string literal" $ do
      parse Parser.string "\"\"" `shouldBe` (Right (CareString ""))
    it "parses a string without escaped character" $ do
      parse Parser.string "\"hello world\"" `shouldBe` (Right (CareString "hello world"))
    it "parses a string with an escaped character" $ do
      parse Parser.string "\"hello\\nworld\"" `shouldBe` (Right (CareString "hello\nworld"))
    it "parses a string with an escaped quote character" $ do
      parse Parser.string "\"hello\\\"world\"" `shouldBe` (Right (CareString "hello\"world"))
