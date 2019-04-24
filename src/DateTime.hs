module DateTime
    ( localTime
    , dateOrDateTime
    )
where

import           FREDValue
import           GenericCombinators
import           Number(frac)
import           Text.Parsec
import           Text.Parsec.String
import           Data.Time
import           Data.Functor


localTime :: Parser FREDValue
localTime = lexeme (LTime <$> try time)

localTimeOrZonedTime :: Parser (TimeOfDay, Maybe TimeZone)
localTimeOrZonedTime = (,) <$> time <*> restTime

time :: Parser TimeOfDay
time = do
    hour <- read <$> count 2 digit
    char ':'
    minutes <- read <$> count 2 digit
    char ':'
    seconds <- count 2 digit
    frac    <- option [] frac
    let time = makeTimeOfDayValid hour minutes (read $ seconds ++ frac)
    fromMaybeP "time" time

restTime :: Parser (Maybe TimeZone)
restTime =
    (char 'Z' *> pure (Just utc)) <|> timeOffSet <|> (ws *> pure Nothing)

timeOffSet :: Parser (Maybe TimeZone)
timeOffSet = do
    sign <- (char '+' <|> char '-')
    hour <- count 2 digit
    string ":"
    minutes <- count 2 digit
    return (Just $ convertToTimeZone sign hour minutes)

convertToTimeZone :: Char -> String -> String -> TimeZone
convertToTimeZone '+' hour minutes =
    hoursToTimeZone (((read hour) * 60) + (read minutes))
convertToTimeZone '-' hour minutes =
    hoursToTimeZone (-1 * ((read hour) * 60) + (read minutes))



dateOrDateTime :: Parser FREDValue
dateOrDateTime = do
    date <- date
    returnDateOrDateTime date <$> rest
  where
    rest :: Parser (Maybe (TimeOfDay, Maybe TimeZone))
    rest = (Just <$> followingTime) <|> (ws $> Nothing)

    returnDateOrDateTime
        :: Day -> Maybe (TimeOfDay, Maybe TimeZone) -> FREDValue
    returnDateOrDateTime date rest = case rest of
        Nothing              -> LDate date
        Just (time, Nothing) -> LDateTime $ LocalTime date time
        Just (time, Just timezone) ->
            DateTime $ ZonedTime (LocalTime date time) timezone

date :: Parser Day
date = do
    year <- read <$> (try (count 4 digit))
    char '-'
    month <- read <$> count 2 digit
    char '-'
    day <- read <$> count 2 digit
    let date = fromGregorianValid year month day
    fromMaybeP "date" date

followingTime :: Parser (TimeOfDay, Maybe TimeZone)
followingTime = char 'T' *> localTimeOrZonedTime


fromMaybeP :: String -> Maybe a -> Parser a
fromMaybeP msg maybe = case maybe of
    Nothing -> unexpected msg
    Just a  -> pure a
