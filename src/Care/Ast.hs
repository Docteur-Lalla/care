module Care.Ast where
  import qualified Text.Parsec as P

  import Care.Value

  -- | A @Located@ value is an object located in a source code. It features a
  -- | start position and an end position.
  data Located a = Located { startPosition :: P.SourcePos
                           , endPosition :: P.SourcePos
                           , value :: a
                           }

  instance Functor Located where
    fmap f loc = loc { value = f (value loc) }

  data Expr = Literal (Located CareValue)
            | BinaryExpr (Located Expr) (Located Expr)
