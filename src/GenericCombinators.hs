module GenericCombinators
    ( stringLiteral
    , lexeme
    , skipWs
    , ws
    )
where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Data.Char
import           Numeric

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
        <|> try (string "\\u" *> unicode 4)
        <|> try (string "\\U" *> unicode 8)


unicode :: Int -> Parser Char
unicode n = do
    digits <- count n hexChar
    pure $ chr (sum (mapIndexed (\n x -> x * 16 ^ n) (reverse digits)))


hexChar :: Parser Int
hexChar = do
    chr <- hexDigit
    let [(value, _)] = readHex [chr]
    return value

mapIndexed :: (Int -> a -> b) -> [a] -> [b]
mapIndexed f xs = go f xs 0  where
    go f []       n = []
    go f (x : xs) n = (f n x) : (go f xs (n + 1))

skipWs :: Parser ()
skipWs = skipMany (oneOf " \t\n,")

ws :: Parser String
ws = many (oneOf " \t\n,")

lexeme :: Parser a -> Parser a
lexeme p = ws *> p <* ws
