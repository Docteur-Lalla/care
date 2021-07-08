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
    s <- fmap normalizeSign $ fromMaybe '+' <$> optionMaybe sign
    int <- fmap ((++) s) integerPart
    opt <- optionMaybe decimalPart
    return $ fromMaybe (readInteger int) $ fmap (readFloat . (++) int) opt
    where normalizeSign :: Char -> String
          normalizeSign '+' = ""
          normalizeSign '-' = "-"
