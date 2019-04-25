module Main where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Data.Maybe
import           Data.Fixed
import           Data.Char
import           Control.Applicative     hiding ( many
                                                , (<|>)
                                                )

import           FREDValue                      ( FREDValue(..) )
import           DateTime                       ( localTime
                                                , dateOrDateTime
                                                )
import           Number                         ( number
                                                , frac
                                                )
import           GenericCombinators             ( stringLiteral
                                                , lexeme
                                                , skipWs
                                                , ws
                                                )

value :: Parser FREDValue
value = tagged <|> atom

atom :: Parser FREDValue
atom =
    object
        <|> array
        <|> dateOrDateTime
        <|> localTime
        <|> symbol
        <|> number
        <|> fredString
        <|> bool
        <|> Main.null


tagged :: Parser FREDValue
tagged = tag <|> voidTag

tag :: Parser FREDValue
tag = lexeme (Tag <$> try tag')
  where
    tag' = do
        tagValue  <- name
        metaValue <- option [] meta
        value     <- atom
        return (tagValue, metaValue, value)

voidTag :: Parser FREDValue
voidTag = lexeme (Tag <$> voidTag')
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
boolTrue = try (string "true") $> True

boolFalse :: Parser Bool
boolFalse = try (string "false") $> False

bool :: Parser FREDValue
bool = lexeme (B <$> (boolTrue <|> boolFalse))

null :: Parser FREDValue
null = lexeme (string "null" *> pure NULL)

fredString :: Parser FREDValue
fredString = lexeme (S <$> stringLiteral)

array :: Parser FREDValue
array =
    lexeme $ A <$> ((lexeme (char '[')) *> many atom <* (lexeme (char ']')))

object :: Parser FREDValue
object = lexeme (O <$> ((char '{') *> many pair <* char '}'))

pair :: Parser (String, FREDValue)
pair = do
    skipWs
    key <- name <|> quotedName
    skipWs
    char ':'
    skipWs
    value <- value
    return (key, value)

name :: Parser String
name = lexeme
    (   (:)
    <$> (oneOf (['a' .. 'z'] ++ ['A' .. 'Z']))
    <*> (many1 (oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])))
    )

quotedName :: Parser String
quotedName = lexeme stringLiteral

symbol :: Parser FREDValue
symbol = Symbol <$> lexeme (char '`' *> name)

main :: IO ()
main = do
    result <- parseFromFile value "test.fred"
    case result of
        Left  err -> print err
        Right xs  -> print xs
