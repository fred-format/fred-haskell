{-|
Module      :  FRED.Value

This module exposes a Data Constructor 'FREDValue' that represents a FRED Document
in haskell.
-}
module FRED.Value
    ( FREDValue(..)
    )
where

import           Data.Time
import qualified Data.ByteString               as B

-- | FREDValue is a data type that represent a FRED Document
data FREDValue =
    B Bool
    | S String
    | A [FREDValue]
    | O [(String, FREDValue)]
    | N (Either Integer Float)
    | Symbol String
    | Blob B.ByteString
    | LDate Day
    | LTime TimeOfDay
    | LDateTime LocalTime -- LocalTime Day TimeOfDay
    | DateTime ZonedTime -- ZonedTime LocalTime TimeZone
    | Tag (String, [(String, FREDValue)], FREDValue)
    | NULL
    deriving Show
