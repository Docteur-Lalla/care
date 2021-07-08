module Care.Parser (number) where
  import Control.Applicative ((<$>), (<*>))
  import Data.Maybe (fromMaybe)

  import Text.Parsec
  import Text.Parsec.Char
  import Text.Parsec.String

  import Care.Value

  integerPart :: Parser String
  integerPart = many1 digit

  decimalPart :: Parser String
  decimalPart = (:) <$> (char '.') <*> (integerPart)

  sign :: Parser Char
  sign = oneOf "-+"

  -- | Parse a number.
  number :: Parser CareValue
  number = do
    s <- normalizeSign <$> option '+' sign
    int <- (s ++) <$> integerPart
    option (readInteger int) $ readFloat . (int ++) <$> decimalPart
    where normalizeSign :: Char -> String
          normalizeSign '+' = ""
          normalizeSign '-' = "-"
