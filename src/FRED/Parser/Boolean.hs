module Fred.Parser.Boolean
    ( bool
    )
where

import           Fred.Value
import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor


boolTrue :: Parser Bool
boolTrue = try (string "true") $> True

boolFalse :: Parser Bool
boolFalse = try (string "false") $> False

bool :: Parser FredAtom
bool = B <$> (boolTrue <|> boolFalse)
