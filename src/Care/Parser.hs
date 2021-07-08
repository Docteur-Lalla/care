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
    int <- integerPart
    opt <- optionMaybe decimalPart
    let sint = s ++ int
    case opt of
      Just dec -> readNum careFloat $ sint ++ dec
      Nothing -> readNum careInteger $ sint
    where readNum :: Read a => (a -> CareValue) -> String -> Parser CareValue
          readNum ctor = return . ctor . read

          normalizeSign :: Char -> String
          normalizeSign '+' = ""
          normalizeSign '-' = "-"
