{-|
Module      :  Fred.Parser

This module has the generic parsers for Fred
-}
module Fred.Parser
    ( document
    )
where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Fred.Value                     ( FredValue(..)
                                                , FredAtom(..)
                                                )
import           Fred.Parser.String             ( name
                                                , ws
                                                , ws1
                                                , symbol
                                                , blob
                                                , fredString
                                                )
import           Fred.Parser.DateTime           ( dateOrDateTime
                                                , localTime
                                                )
import           Fred.Parser.Boolean            ( bool )
import           Fred.Parser.Number             ( number )


document :: Parser FredValue
document = do
    skipMany comment
    doc <- stream <|> value
    skipMany comment
    eof
    return doc
  where
    stream :: Parser FredValue
    stream =
        Stream
            <$> (string "#." *> ws *> value `sepBy` (ws *> string "#." <* ws))

comment :: Parser FredAtom
comment = char ';' *> manyTill anyChar newline $> NULL

value :: Parser FredValue
value = tagged <|> (NonTag <$> atom)

atom :: Parser FredAtom
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
        <|> Fred.Parser.null


tagged :: Parser FredValue
tagged = tag <|> voidTag

tag :: Parser FredValue
tag = Tag <$> try tag'
  where
    tag' = do
        tagValue <- name
        ws
        metaValue <- option [] meta
        ws
        value <- value
        return (tagValue, metaValue, value)

voidTag :: Parser FredValue
voidTag = Tag <$> voidTag'
  where
    voidTag' = do
        char '('
        ws
        tagValue <- name
        ws
        metaValue <- manyMetaItem
        char ')'
        return (tagValue, metaValue, NonTag NULL)

meta :: Parser [(String, FredAtom)]
meta = char '(' *> ws *> manyMetaItem <* char ')'

manyMetaItem :: Parser [(String, FredAtom)]
manyMetaItem = metaItem `sepEndBy` ws1

metaItem :: Parser (String, FredAtom)
metaItem = meta'

meta' :: Parser (String, FredAtom)
meta' = do
    key <- name
    ws
    char '='
    ws
    value <- atom
    return (key, value)

null :: Parser FredAtom
null = string "null" $> NULL

array :: Parser FredAtom
array = A <$> (char '[' *> ws *> value `sepEndBy` ws1 <* char ']')

object :: Parser FredAtom
object = O <$> (char '{' *> ws *> pair `sepEndBy` ws1 <* char '}')

pair :: Parser (String, FredValue)
pair = do
    key <- name
    ws
    char ':'
    ws
    value <- value
    return (key, value)
