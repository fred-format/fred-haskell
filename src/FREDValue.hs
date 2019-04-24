module FREDValue
    ( FREDValue(..)
    )
where

import           Data.Time

data FREDValue =
    B Bool
    | S String
    | A [FREDValue]
    | O [(String, FREDValue)]
    | N (Either Integer Float)
    | LDate Day
    | LTime TimeOfDay
    | LDateTime LocalTime -- LocalTime Day TimeOfDay
    | DateTime ZonedTime -- ZonedTime LocalTime TimeZone
    | Tag (String, [(String, FREDValue)], FREDValue)
    | NULL
    deriving Show
