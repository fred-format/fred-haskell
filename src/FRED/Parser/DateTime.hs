module Fred.Parser.DateTime
    ( localTime
    , dateOrDateTime
    )
where

import           Fred.Value
import           Fred.Parser.String
import           Fred.Parser.Number             ( frac )
import           Text.Parsec
import           Text.Parsec.String
import           Data.Time
import           Data.Functor


localTime :: Parser FredAtom
localTime = (LTime <$> try time)

localTimeOrZonedTime :: Parser (TimeOfDay, Maybe TimeZone)
localTimeOrZonedTime = (,) <$> time <*> option Nothing restTime

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
restTime = char 'Z' $> Just utc <|> timeOffSet

timeOffSet :: Parser (Maybe TimeZone)
timeOffSet = do
    sign <- char '+' <|> char '-'
    hour <- count 2 digit
    string ":"
    minutes <- count 2 digit
    return (Just $ convertToTimeZone sign hour minutes)

convertToTimeZone :: Char -> String -> String -> TimeZone
convertToTimeZone '+' hour minutes =
    hoursToTimeZone (((read hour) * 60) + (read minutes))
convertToTimeZone '-' hour minutes =
    hoursToTimeZone (-1 * ((read hour) * 60) + (read minutes))



dateOrDateTime :: Parser FredAtom
dateOrDateTime = do
    date <- try date
    returnDateOrDateTime date <$> option Nothing rest
  where
    rest :: Parser (Maybe (TimeOfDay, Maybe TimeZone))
    rest = Just <$> followingTime

    returnDateOrDateTime :: Day -> Maybe (TimeOfDay, Maybe TimeZone) -> FredAtom
    returnDateOrDateTime date rest = case rest of
        Nothing              -> LDate date
        Just (time, Nothing) -> LDateTime $ LocalTime date time
        Just (time, Just timezone) ->
            DateTime $ ZonedTime (LocalTime date time) timezone

date :: Parser Day
date = do
    year <- read <$> ((count 4 digit))
    char '-'
    month <- read <$> count 2 digit
    char '-'
    day <- read <$> count 2 digit
    let date = fromGregorianValid year month day
    fromMaybeP "date" date

followingTime :: Parser (TimeOfDay, Maybe TimeZone)
followingTime = (char 'T' <|> char '_') *> localTimeOrZonedTime


fromMaybeP :: String -> Maybe a -> Parser a
fromMaybeP msg maybe = case maybe of
    Nothing -> unexpected msg
    Just a  -> pure a
