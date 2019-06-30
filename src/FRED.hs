{-|
Module      :  Fred

This module exposes a function 'Fred.parse' that receives a Fred Document
and return a representation as a Haskell Value.
-}

module Fred
  ( Fred.parse
  , Fred.minify
  )
where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Data.Maybe
import           Data.Fixed
import           Data.Either
import           Data.List
import qualified Data.ByteString.Char8         as BC
import           System.Environment

import           Data.Char
import           Control.Applicative     hiding ( many
                                                , (<|>)
                                                )

import           Fred.Value                     ( FredDocument(..)
                                                , FredValue(..)
                                                , FredAtom(..)
                                                )
import           Fred.Parser.DateTime           ( localTime
                                                , dateOrDateTime
                                                )
import           Fred.Parser.Number             ( number
                                                , frac
                                                )
import           Fred.Parser.String             ( stringLiteral
                                                , blobString
                                                , name
                                                , ws
                                                , ws1
                                                )
document :: Parser FredDocument
document = do
  skipMany comment
  doc <- (Stream <$> stream) <|> (Doc <$> value)
  skipMany comment
  eof
  return doc
 where
  stream :: Parser [FredValue]
  stream = string "---" *> ws *> value `endBy` (ws *> string "---" <* ws)

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
    <|> Fred.null


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
    value <- atom
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
    return (tagValue, metaValue, NULL)

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

boolTrue :: Parser Bool
boolTrue = try (string "true") $> True

boolFalse :: Parser Bool
boolFalse = try (string "false") $> False

bool :: Parser FredAtom
bool = B <$> (boolTrue <|> boolFalse)

null :: Parser FredAtom
null = string "null" $> NULL

fredString :: Parser FredAtom
fredString = S <$> stringLiteral

array :: Parser FredAtom
array = A <$> (char '[' *> ws *> atom `sepEndBy` ws1 <* char ']')

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

symbol :: Parser FredAtom
symbol = Symbol <$> (char '$' *> name)

blob :: Parser FredAtom
blob = Blob . BC.pack <$> (char '#' *> blobString)

-- | Parse a Fred Document.
parse :: String -> Either ParseError FredDocument
parse = Text.Parsec.parse document ""

minify :: FredDocument -> IO ()
minify fred = writeFile "dump.fred" (dump fred)




-- Dump TypeClass for Fred
class Dump a where
  dump :: a -> String

instance Dump FredDocument where
  dump (Stream values) = 
    (Data.List.foldl dumpStream "" values) ++ "---"
  dump (Doc value) = dump value

dumpStream :: String -> FredValue -> String
dumpStream acc value = acc ++ "---" ++ dump value

instance Dump FredValue where
  dump (Tag (tag, meta, NULL)) = "(" ++ tag ++ dumpMeta meta ++ ")"
  dump (Tag (tag, meta, atom)) =
    tag ++ "(" ++ dumpMeta meta ++ ")" ++ dump atom
  dump (NonTag atom) = dump atom

dumpMeta :: [(String, FredAtom)] -> String
dumpMeta meta = drop 1 (Data.List.foldl dumpMetaItem "" meta)
 where
  dumpMetaItem :: String -> (String, FredAtom) -> String
  dumpMetaItem acc (name, atom) = acc ++ " " ++ name ++ "=" ++ (dump atom)

instance Dump FredAtom where
  dump (B         True         ) = "true"
  dump (B         False        ) = "false"
  dump (S         string       ) = "\"" ++ string ++ "\""
  dump (A array) = "[" ++ drop 1 (Data.List.foldl dumpArr "" array) ++ "]"
  dump (O object) = "{" ++ drop 1 (Data.List.foldl dumpObj "" object) ++ "}"
  dump (N         (Left  int  )) = show int
  dump (N         (Right float)) = show float
  dump (Symbol    symb         ) = "$" ++ symb
  dump (Blob      str          ) = "#" ++ "\"" ++ BC.unpack str ++ "\"" 
  dump (LDate     day          ) = show day
  dump (LTime     time         ) = show time
  dump (LDateTime localTime    ) = show localTime
  dump (DateTime  zonedTime    ) = show zonedTime
  dump NULL                      = "null"


dumpArr :: String -> FredAtom -> String
dumpArr acc atom = acc ++ " " ++ dump atom

dumpObj :: String -> (String, FredValue) -> String
dumpObj acc (key, value) 
  | ' ' `elem` key = acc ++ " " ++ "`" ++ key ++ "`" ++ ":" ++ dump value
  | otherwise = acc ++ " " ++ key ++ ":" ++ dump value
