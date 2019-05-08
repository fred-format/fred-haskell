{-|
Module      :  FRED

This module exposes a function 'FRED.parse' that receives a FRED Document
and return a representation as a Haskell Value.
-}

module FRED
    ( FRED.parse
    )
where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Data.Maybe
import           Data.Fixed
import           Data.List
import qualified Data.ByteString.Char8         as BC

import           Data.Char
import           Control.Applicative     hiding ( many
                                                , (<|>)
                                                )

import           FRED.Value                     ( FREDValue(..) )
import           FRED.Parser.DateTime           ( localTime
                                                , dateOrDateTime
                                                )
import           FRED.Parser.Number             ( number
                                                , frac
                                                )
import           FRED.Parser.String             ( stringLiteral
                                                , blobString
                                                , name
                                                , ws
                                                , ws1
                                                )
document :: Parser FREDValue
document = do
    skipMany comment
    doc <- (A <$> stream) <|> value
    skipMany comment
    return doc
  where
    stream :: Parser [FREDValue]
    stream = string "---" *> ws *> value `endBy` (ws *> string "---" <* ws)

comment :: Parser FREDValue
comment = char ';' *> manyTill anyChar newline $> NULL

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
        <|> FRED.null


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
manyMetaItem = metaItem `sepEndBy` ws1

metaItem :: Parser (String, FREDValue)
metaItem = meta'

meta' :: Parser (String, FREDValue)
meta' = do
    key <- name
    ws
    char '='
    ws
    value <- atom
    return (key, value)

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
array = A <$> (char '[' *> ws *> atom `sepEndBy` ws1 <* char ']')

object :: Parser FREDValue
object = O <$> (char '{' *> ws *> pair `sepEndBy` ws1 <* char '}')

pair :: Parser (String, FREDValue)
pair = do
    key <- name
    ws
    char ':'
    ws
    value <- value
    return (key, value)

symbol :: Parser FREDValue
symbol = Symbol <$> (char '$' *> name)

blob :: Parser FREDValue
blob = Blob . BC.pack <$> (char '#' *> blobString)

-- | Parse a FRED Document.
parse :: String -> Either ParseError FREDValue
parse = Text.Parsec.parse document ""
