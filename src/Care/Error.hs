module Care.Error where
  import Control.Monad.Except (ExceptT, throwError)
  import Text.Parsec (SourcePos, sourceColumn, sourceLine, sourceName)

  data ErrorType = UnboundVariable String
                 | CustomError String

  instance Show ErrorType where
    show (UnboundVariable var) = "unbound variable: \"" ++ var ++ "\""
    show (CustomError err) = err

  newtype CareError = CareError (SourcePos, ErrorType)

  instance Show CareError where
    show (CareError (pos, err)) = show pos ++ ": " ++ show err

  -- |Type for operations that can fail.
  type Result = Either CareError
  -- |Type for IO operations that can fail.
  type IOResult = ExceptT CareError IO

  liftResult :: Result a -> IOResult a
  liftResult (Left err) = throwError err
  liftResult (Right val) = return val
