module Main where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Control.Applicative     hiding ( many
                                                , (<|>)
                                                )
import           Numeric
import           Data.Char
import           Combinators                    ( stringLiteral
                                                , lexeme
                                                , skipWs
                                                )

data FREDValue =
    B Bool
    | S String
    | A [FREDValue]
    | O [(String, FREDValue)]
    | N (Either Integer Float)
    | D (Integer, Integer, Integer)
    | T (String, [(String, FREDValue)], FREDValue)
    | NULL
    deriving Show

value :: Parser FREDValue
value = try tagged <|> atom

atom :: Parser FREDValue
atom =
    try array
        <|> try object
        <|> try date
        <|> try number
        <|> try fredString
        <|> try bool
        <|> try Main.null
        <*  spaces


tagged :: Parser FREDValue
tagged = tag <|> voidTag

tag :: Parser FREDValue
tag = lexeme (T <$> tag')
  where
    tag' = do
        tagValue  <- name
        metaValue <- option [] meta
        value     <- atom
        return (tagValue, metaValue, value)

voidTag :: Parser FREDValue
voidTag = lexeme (T <$> voidTag')
  where
    voidTag' = do
        char '('
        tagValue  <- name
        metaValue <- many metaItem
        char ')'
        return (tagValue, metaValue, NULL)

meta :: Parser [(String, FREDValue)]
meta = char '(' *> many metaItem <* char ')'

metaItem :: Parser (String, FREDValue)
metaItem = lexeme (try meta' <|> voidMeta)

meta' :: Parser (String, FREDValue)
meta' = do
    key <- name
    skipWs
    char '='
    skipWs
    value <- atom
    return (key, value)

voidMeta :: Parser (String, FREDValue)
voidMeta = do
    value <- name
    return (value, NULL)

boolTrue :: Parser Bool
boolTrue = string "true" *> pure True

boolFalse :: Parser Bool
boolFalse = string "false" *> pure False

bool :: Parser FREDValue
bool = lexeme (B <$> (boolTrue <|> boolFalse))

null :: Parser FREDValue
null = lexeme (string "null" *> pure NULL)

date :: Parser FREDValue
date = lexeme (D <$> date)
  where
    date :: Parser (Integer, Integer, Integer)
    date = do
        year <- read <$> count 4 digit
        char '-'
        month <- read <$> count 2 digit
        char '-'
        day <- read <$> count 2 digit
        return (year, month, day)

fredString :: Parser FREDValue
fredString = lexeme (S <$> stringLiteral)

number :: Parser FREDValue
number =
    lexeme (N <$> (toNumber <$> int <*> (option [] frac) <*> (option [] expo)))

toNumber :: [Char] -> [Char] -> [Char] -> Either Integer Float
toNumber int []   []                 = Left (read int)
toNumber int frac []                 = Right ((read int) + (realFrac frac))
toNumber int []   (e : '+' : digits) = Left ((read int) ^ (read digits))
toNumber int []   (e : '-' : digits) = Right ((read int) ^ (read digits))
toNumber int frac exp = Right (((read int) + (realFrac frac)) ^ realExp exp)

realFrac :: [Char] -> Float
realFrac frac = read ('0' : frac)

realExp :: [Char] -> Int
realExp (e : '+' : digits) = read digits
realExp (e       : digits) = read digits

int :: Parser [Char]
int = many1 digit

frac :: Parser [Char]
frac = ((:) <$> char '.' <*> many1 digit)

expo :: Parser [Char]
expo =
    (do
            e      <- char 'E'
            sign   <- sign
            digits <- many1 digit
            return (e : sign ++ digits)
        )
        <|> (do
                e      <- char 'e'
                sign   <- sign
                digits <- many1 digit
                return (e : sign ++ digits)
            )
-- <|> ((:) <$> char 'E' <*> ((++) <$> sign <*> many1 digit))
-- <|> ((:) <$> char 'e' <*> ((++) <$> sign <*> many1 digit))

sign :: Parser [Char]
sign = string "" <|> string "-" <|> string "+"

array :: Parser FREDValue
array = A <$> (lexeme (char '[') *> many atom <* (char ']'))

object :: Parser FREDValue
object = O <$> (lexeme (char '{') *> many pair <* char '}')

pair :: Parser (String, FREDValue)
pair = do
    key <- name <|> quotedName
    skipWs
    char ':'
    skipWs
    value <- value
    return (key, value)

name :: Parser String
name = lexeme (many1 (oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])))

quotedName :: Parser String
quotedName = lexeme stringLiteral

main :: IO ()
main = do
    result <- parseFromFile value "test.fred"
    case result of
        Left  err -> print err
        Right xs  -> print xs
