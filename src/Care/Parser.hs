module Care.Parser (number, string) where
  import Control.Applicative ((<$>), (<*>))
  import Data.Maybe (fromMaybe)

  import Text.Parsec hiding (string)
  import qualified Text.Parsec as P (string)
  import Text.Parsec.Char hiding (string)
  import Text.Parsec.String

  import Care.Value

  integerPart :: Parser String
  integerPart = many1 digit

  decimalPart :: Parser String
  decimalPart = (:) <$> (char '.') <*> (integerPart)

  sign :: Parser Char
  sign = oneOf "-+"

  -- | Parse a decimal number.
  decimalNumber :: Parser CareValue
  decimalNumber = do
    s <- normalizeSign <$> option '+' sign
    int <- (s ++) <$> integerPart
    option (readInteger int) $ readFloat . (int ++) <$> decimalPart

  normalizeSign :: Char -> String
  normalizeSign '+' = ""
  normalizeSign '-' = "-"

  applyWhen :: Bool -> (a -> a) -> a -> a
  applyWhen True = ($)
  applyWhen False = flip const

  -- | Parse a '0x' prefixed hexadecimal number.
  hexadecimalNumber :: Parser CareValue
  hexadecimalNumber = do
    sign <- (== '-') <$> option '+' sign
    careInteger . applyWhen sign negate . read <$> literal
      where literal :: Parser String
            literal = P.string "0x" `mappend` many1 hexDigit

  -- | Parse a number literal.
  number :: Parser CareValue
  number = try hexadecimalNumber <|> decimalNumber

  -- | Parse a string literal.
  string :: Parser CareValue
  string = CareString <$> between (char '"') (char '"') stringContents
    where stringContents = many (try escapable <|> noneOf "\"")
          escapable = escape <$> (char '\\' >> oneOf "\\\"nrt")
          escape '\\' = '\\'
          escape '"' = '"'
          escape 'n' = '\n'
          escape 'r' = '\r'
          escape 't' = '\t'
