{-|
Module      :  Fred

This module exposes a function 'Fred.parse' that receives a Fred Document
and return a representation as a Haskell Value.
-}

module Fred
  ( Fred.parse
  , Fred.parseFromFile
  , Fred.minify
  )
where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Either
import           Data.List
import           Data.Time
import           Data.Time.Format
import           Data.Time.Format.ISO8601       ( iso8601Show )
import qualified Data.ByteString.Char8         as BC
import           System.Environment
import           Data.Char

import           Fred.Parser                    ( document )
import           Fred.Value                     ( FredValue(..)
                                                , FredAtom(..)
                                                )
import           Fred.Parser.Boolean            ( bool )
import           Fred.Parser.DateTime           ( localTime
                                                , dateOrDateTime
                                                )
import           Fred.Parser.Number             ( number
                                                , frac
                                                )
import           Fred.Parser.String             ( fredString
                                                , blob
                                                , symbol
                                                , name
                                                , ws
                                                , ws1
                                                )


-- | Parse a Fred Document.
parse :: String -> Either ParseError FredValue
parse = Text.Parsec.parse document ""

parseFromFile fname = do
  input <- readFile fname
  return (runParser document () fname input)

minify :: FredValue -> String
minify = dump

-- Dump TypeClass for Fred
class Dump a where
  dump :: a -> String


instance Dump FredValue where
  dump (Tag (tag, meta, NonTag NULL)) =
    "(" ++ tag ++ " " ++ dumpMeta meta ++ ")"
  dump (Tag (tag, [], atom)) = tag ++ " " ++ dump atom
  dump (Tag (tag, meta, atom)) =
    tag ++ " " ++ "(" ++ dumpMeta meta ++ ")" ++ " " ++ dump atom
  dump (NonTag atom) = dump atom

dumpMeta :: [(String, FredAtom)] -> String
dumpMeta meta = drop 1 (Data.List.foldl dumpMetaItem "" meta)
 where
  dumpMetaItem :: String -> (String, FredAtom) -> String
  dumpMetaItem acc (name, atom) = acc ++ " " ++ name ++ "=" ++ (dump atom)

instance Dump FredAtom where
  dump (B         True           ) = "true"
  dump (B         False          ) = "false"
  dump (S         string         ) = "\"" ++ string ++ "\""
  dump (A array) = "[" ++ drop 1 (Data.List.foldl dumpArr "" array) ++ "]"
  dump (O object) = "{" ++ drop 1 (Data.List.foldl dumpObj "" object) ++ "}"
  dump (N         (Left  int  )  ) = show int
  dump (N         (Right float)  ) = show float
  dump (Symbol    symb           ) = "$" ++ symb
  dump (Blob      str            ) = "`" ++ BC.unpack str ++ "`"
  dump (LDate     day            ) = show day
  dump (LTime     time           ) = show time
  dump (LDateTime (LocalTime d t)) = (showGregorian d) ++ "T" ++ (show t)
  dump (DateTime (ZonedTime (LocalTime d t) zone)) =
    (showGregorian d) ++ "T" ++ (show t) ++ (iso8601Show zone)
  dump NULL = "null"


dumpArr :: String -> FredValue -> String
dumpArr acc atom = acc ++ " " ++ dump atom

dumpArrS :: String -> FredValue -> String
dumpArrS acc atom = acc ++ " #. " ++ dump atom

dumpObj :: String -> (String, FredValue) -> String
dumpObj acc (key, value)
  | ' ' `elem` key = acc ++ " " ++ "`" ++ key ++ "`" ++ ":" ++ dump value
  | otherwise      = acc ++ " " ++ key ++ ":" ++ dump value
