module GenericCombinators
    ( stringLiteral
    , variable
    , quotedVariable
    , lexeme
    , ws
    )
where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Data.Char
import           Numeric


variable :: Parser String
variable = (:) <$> variableChar <*> many variableCharWithDigits


variableChar :: Parser Char
variableChar =
    oneOf (['>' .. 'Z'] ++ ['a' .. 'z'] ++ ['!', '%', '&', '.', '<', '^', '_'])

variableCharWithDigits :: Parser Char
variableCharWithDigits = oneOf
    (  ['>' .. 'Z']
    ++ ['a' .. 'z']
    ++ ['!', '%', '&', '.', '<', '^', '_']
    ++ ['0' .. '9']
    )

quotedVariable :: Parser String
quotedVariable = char '`' *> many character' <* char '`'

character' :: Parser Char
character' =
    noneOf "`\\"
        <|> try (string "\\n" $> '\n')
        <|> try (string "\\`" $> '`')
        <|> try (string "\\\\" $> '\\')
        <|> try (string "\\/" $> '/')
        <|> try (string "\\b" $> '\b')
        <|> try (string "\\f" $> '\f')
        <|> try (string "\\r" $> '\r')
        <|> try (string "\\t" $> '\t')
        <|> try (string "\\x" *> unicode 2)
        <|> try (string "\\u" *> unicode 4)
        <|> try (string "\\U" *> unicode 8)

stringLiteral :: Parser String
stringLiteral = char '"' *> many character <* char '"'

character :: Parser Char
character =
    noneOf "\"\\"
        <|> try (string "\\n" $> '\n')
        <|> try (string "\\\"" $> '\"')
        <|> try (string "\\\\" $> '\\')
        <|> try (string "\\/" $> '/')
        <|> try (string "\\b" $> '\b')
        <|> try (string "\\f" $> '\f')
        <|> try (string "\\r" $> '\r')
        <|> try (string "\\t" $> '\t')
        <|> try (string "\\x" *> unicode 2)
        <|> try (string "\\u" *> unicode 4)
        <|> try (string "\\U" *> unicode 8)

unicode :: Int -> Parser Char
unicode n = do
    digits <- count n hexDigit
    let [(hex, _)] = readHex digits
    failIfOutUnicode hex

failIfOutUnicode :: Int -> Parser Char
failIfOutUnicode n | n > 1114111 = unexpected "outside unicode"
                   | otherwise   = pure (chr n)

ws :: Parser String
ws = many (oneOf " \t\n,")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws
