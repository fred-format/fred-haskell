{-|
Module      :  Fred.Value

This module exposes a Data Constructor 'FredValue' that represents a Fred Document
in haskell.
-}
module Fred.Value
    ( FredValue(..)
    , FredAtom(..)
    )
where

import           Data.Time
import           Data.List
import qualified Data.ByteString               as B
import qualified Data.ByteString.Char8         as BC


data FredValue =
    Tag (String, [(String, FredAtom)], FredValue) | NonTag FredAtom
    deriving (Show, Eq)

data FredAtom =
    B Bool
    | S String
    | A [FredValue]
    | O [(String, FredValue)]
    | N (Either Integer Double)
    | Symbol String
    | Blob B.ByteString
    | LDate Day
    | LTime TimeOfDay
    | LDateTime LocalTime -- LocalTime Day TimeOfDay
    | DateTime ZonedTime -- ZonedTime LocalTime TimeZone
    | NULL
    deriving (Show, Eq)

instance (Eq ZonedTime) where
    (==) x y = zonedTimeToLocalTime x == zonedTimeToLocalTime y
    (/=) x y = zonedTimeToLocalTime x /= zonedTimeToLocalTime y
