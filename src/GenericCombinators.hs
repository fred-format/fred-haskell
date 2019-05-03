module GenericCombinators
    ( stringLiteral
    , blobString
    , name
    , ws
    , ws1
    )
where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Data.Char
import           Numeric

name :: Parser String
name = variable <|> quotedVariable

variable :: Parser String
variable = (++) <$> many1 variableChar <*> many variableCharWithDigits

variableChar :: Parser Char
variableChar =
    noneOf ("#\"`$:;{}[]=()\t\r\n ," ++ ['0' .. '9'])

variableCharWithDigits :: Parser Char
variableCharWithDigits = noneOf "#\"`$:;{}[]=()\t\r\n ,"

quotedVariable :: Parser String
quotedVariable = char '`' *> many quotedVariableChar <* char '`'

quotedVariableChar :: Parser Char
quotedVariableChar =
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

blobString :: Parser String
blobString = char '"' *> many blobChar <* char '"'

blobChar :: Parser Char
blobChar = noneOf "\"\\\\u\\U"
            <|> try (string "\\n" $> '\n')
            <|> try (string "\\\"" $> '\"')
            <|> try (string "\\\\" $> '\\')
            <|> try (string "\\/" $> '/')
            <|> try (string "\\b" $> '\b')
            <|> try (string "\\f" $> '\f')
            <|> try (string "\\r" $> '\r')
            <|> try (string "\\t" $> '\t')
            <|> try (string "\\x" *> unicode 2)


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

ws :: Parser [String]
ws =  many (many1 (oneOf " \t\n\r,") <|> (char ';' *> manyTill anyChar newline))

ws1 :: Parser [String]
ws1 = many1 (many1 (oneOf " \t\n\r,") <|> (char ';' *> manyTill anyChar newline))