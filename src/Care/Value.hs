module Care.Value where
  import Data.List (intercalate)
  import Data.Ratio (numerator, denominator)
  import Data.Map.Strict (Map, toList)

  -- | The Number data type in care.
  --
  -- The language implicitely casts integers to floating-point numbers, however
  -- it tries to keep integer integrity as much as possible. For this reason,
  -- the @Number@ type keeps the two @Integer@ and @Floating@ constructors. As
  -- @Number@ implements @Fractional@ and @Num@, it is possible to use it like
  -- any other number type:
  --
  -- > someNumber :: Number
  -- > someNumber = Integer 5 + Floating 6.5 / Floating 2.0
  -- >
  -- > someNumber == Floating 8.25
  data Number = Integer Int
              | Floating Double

  instance Show Number where
    show (Integer i) = show i
    show (Floating f) = show f

  instance Eq Number where
    (Integer a) == (Integer b) = a == b
    (Floating a) == (Floating b) = a == b
    (Integer a) == (Floating b) = fromIntegral a == b
    (Floating a) == (Integer b) = a == fromIntegral b

  instance Ord Number where
    compare a b = compare (toDouble a) (toDouble b)
      where toDouble (Floating f) = f
            toDouble (Integer i) = fromIntegral i

  instance Fractional Number where
    (/) (Integer a) (Integer b) = Integer $ a `div` b
    (/) (Integer a) (Floating b) = Floating $ fromIntegral a / b
    (/) (Floating a) (Integer b) = Floating $ a / fromIntegral b
    (/) (Floating a) (Floating b) = Floating $ a / b

    fromRational r
      | denominator r == 1 = Integer $ fromIntegral $ numerator r
      | otherwise = Floating $ fromRational r

  instance Num Number where
    (+) (Integer a) (Integer b) = Integer $ a + b
    (+) (Floating a) (Integer b) = Floating $ a + fromIntegral b
    (+) (Integer a) (Floating b) = Floating $ fromIntegral  a + b
    (+) (Floating a) (Floating b) = Floating $ a + b

    (*) (Integer a) (Integer b) = Integer $ a * b
    (*) (Floating a) (Integer b) = Floating $ a * fromIntegral b
    (*) (Integer a) (Floating b) = Floating $ fromIntegral a * b
    (*) (Floating a) (Floating b) = Floating $ a * b

    negate (Integer i) = Integer $ -i
    negate (Floating f) = Floating $ -f

    abs (Integer i)
      | i < 0 = Integer (-i)
      | otherwise = Integer i
    abs (Floating f)
      | f < 0 = Floating (-f)
      | otherwise = Floating f

    signum (Integer i)
      | i < 0 = Integer (-1)
      | i == 0 = Integer 0
      | otherwise = Integer 1
    signum (Floating f)
      | f < 0 = Floating (-1)
      | f == 0 = Floating 0
      | otherwise = Floating 1

    fromInteger i = Integer (fromIntegral i)

  -- | A value in care language.
  data CareValue = CareNumber Number
                 | CareString String
                 | CareArray (Map String CareValue)
                 deriving Eq

  instance Show CareValue where
    show (CareNumber n) = show n
    show (CareString s) = s
    show (CareArray a) = '{' : values ++ "}"
      where values = intercalate ", " $ fmap showPair $ toList a
            showPair (key, val) = key ++ ": " ++ show val

  -- | Helper to create a care integer.
  careInteger :: Int -> CareValue
  careInteger = CareNumber . Integer

  -- | Helper to create a care floating-point number.
  careFloat :: Double -> CareValue
  careFloat = CareNumber . Floating

  readNumber :: Read a => (a -> CareValue) -> String -> CareValue
  readNumber ctor = ctor . read

  readInteger :: String -> CareValue
  readInteger = readNumber careInteger

  readFloat :: String -> CareValue
  readFloat = readNumber careFloat

  -- | Helper to get a haskell string from a care value if it is a care string.
  stringOf :: CareValue -> Maybe String
  stringOf (CareString s) = Just s
  stringOf _ = Nothing
