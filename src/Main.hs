module Main where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Data.Maybe
import           Data.Fixed
import qualified Data.ByteString.Char8         as BC

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
                                                , variable
                                                , quotedVariable
                                                , lexeme
                                                , ws
                                                )
document :: Parser FREDValue
document = (A <$> stream) <|> value
    where
        stream :: Parser [FREDValue]
        stream = string "---" *> ws  *> value `endBy` ( ws *> string "---" <* ws)

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
        <|> blob
        <|> fredString
        <|> bool
        <|> Main.null


tagged :: Parser FREDValue
tagged = tag <|> voidTag

tag :: Parser FREDValue
tag = Tag <$> try tag'
  where
    tag' = do
        tagValue <- name
        ws
        metaValue <- option [] meta
        ws
        value <- atom
        return (tagValue, metaValue, value)

voidTag :: Parser FREDValue
voidTag = Tag <$> voidTag'
  where
    voidTag' = do
        char '('
        ws
        tagValue <- name
        ws
        metaValue <- manyMetaItem
        char ')'
        return (tagValue, metaValue, NULL)

meta :: Parser [(String, FREDValue)]
meta = char '(' *> ws *> manyMetaItem <* char ')'

manyMetaItem :: Parser [(String, FREDValue)]
manyMetaItem = metaItem `sepEndBy` many1 (oneOf " \t\n,")

metaItem :: Parser (String, FREDValue)
metaItem = try meta' <|> voidMeta

meta' :: Parser (String, FREDValue)
meta' = do
    key <- name
    ws
    char '='
    ws
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
bool = B <$> (boolTrue <|> boolFalse)

null :: Parser FREDValue
null = string "null" $> NULL

fredString :: Parser FREDValue
fredString = S <$> stringLiteral

array :: Parser FREDValue
array =
    A <$> (char '[' *> ws *> atom `sepEndBy` many1 (oneOf " \t\n,") <* char ']')

object :: Parser FREDValue
object =
    O <$> (char '{' *> ws *> pair `sepEndBy` many1 (oneOf " \t\n,") <* char '}')

pair :: Parser (String, FREDValue)
pair = do
    key <- name
    ws
    char ':'
    ws
    value <- value
    return (key, value)

name :: Parser String
name = variable <|> quotedVariable

symbol :: Parser FREDValue
symbol = Symbol <$> (char '$' *> name)

blob :: Parser FREDValue
blob = Blob . BC.pack <$> (char '#' *> stringLiteral)

main :: IO ()
main = do
    result <- parseFromFile document "test.fred"
    case result of
        Left  err -> print err
        Right xs  -> print xs
