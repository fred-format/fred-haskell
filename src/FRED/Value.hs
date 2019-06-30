{-|
Module      :  Fred.Value

This module exposes a Data Constructor 'FredValue' that represents a Fred Document
in haskell.
-}
module Fred.Value
    ( FredDocument(..)
    , FredValue(..)
    , FredAtom(..)
    )
where

import           Data.Time
import           Data.List
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC


-- | FredDocument is a data type that represent a Fred Document
data FredDocument =
    Stream [FredValue]
    | Doc FredValue
    deriving Show

data FredValue =
    Tag (String, [(String, FredAtom)], FredAtom) | NonTag FredAtom
    deriving Show

data FredAtom =
    B Bool
    | S String
    | A [FredAtom]
    | O [(String, FredValue)]
    | N (Either Integer Float)
    | Symbol String
    | Blob B.ByteString
    | LDate Day
    | LTime TimeOfDay
    | LDateTime LocalTime -- LocalTime Day TimeOfDay
    | DateTime ZonedTime -- ZonedTime LocalTime TimeZone
    | NULL
    deriving Show
