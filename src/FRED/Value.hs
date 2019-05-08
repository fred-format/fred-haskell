module FRED.Value
    ( FREDValue(..)
    )
where

import           Data.Time
import qualified Data.ByteString               as B

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
