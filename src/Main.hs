module Main where

import           Text.Parsec
import           Text.Parsec.String
import           Data.Functor
import           Data.Maybe
import           Data.Fixed
import           Control.Applicative     hiding ( many
                                                , (<|>)
                                                )
import           Numeric
import           Data.Char
import           Data.Time
import           Combinators                    ( stringLiteral
                                                , lexeme
                                                , skipWs
                                                , ws
                                                )

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

value :: Parser FREDValue
value = tagged <|> atom

atom :: Parser FREDValue
atom =
    object
        <|> array
        <|> dateOrDateTime
        <|> localTime
        <|> number
        <|> fredString
        <|> bool
        <|> Main.null

tagged :: Parser FREDValue
tagged = tag <|> voidTag

tag :: Parser FREDValue
tag = lexeme (Tag <$> try tag')
  where
    tag' = do
        tagValue  <- name
        metaValue <- option [] meta
        value     <- atom
        return (tagValue, metaValue, value)

voidTag :: Parser FREDValue
voidTag = lexeme (Tag <$> voidTag')
  where
    voidTag' = do
        char '('
        tagValue  <- name
        metaValue <- many metaItem
        char ')'
        return (tagValue, metaValue, NULL)

meta :: Parser [(String, FREDValue)]
meta = char '(' *> many metaItem <* char ')'

metaItem :: Parser (String, FREDValue)
metaItem = lexeme (try meta' <|> voidMeta)

meta' :: Parser (String, FREDValue)
meta' = do
    key <- name
    skipWs
    char '='
    skipWs
    value <- atom
    return (key, value)

voidMeta :: Parser (String, FREDValue)
voidMeta = do
    value <- name
    return (value, NULL)

boolTrue :: Parser Bool
boolTrue = try (string "true") $> True

boolFalse :: Parser Bool
boolFalse = try (string "false") $> False

bool :: Parser FREDValue
bool = lexeme (B <$> (boolTrue <|> boolFalse))

null :: Parser FREDValue
null = lexeme (string "null" *> pure NULL)

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

fredString :: Parser FREDValue
fredString = lexeme (S <$> stringLiteral)

number :: Parser FREDValue
number =
    lexeme (N <$> (toNumber <$> int <*> (option [] frac) <*> (option [] expo)))


toNumber :: [Char] -> [Char] -> [Char] -> Either Integer Float
toNumber int []   []                 = Left (read int)
toNumber int frac []                 = Right ((read int) + (realFrac frac))
toNumber int []   (e : '+' : digits) = Left ((read int) ^ (read digits))
toNumber int []   (e : '-' : digits) = Right ((read int) ^ (read digits))
toNumber int frac exp = Right (((read int) + (realFrac frac)) ^ realExp exp)

realFrac :: [Char] -> Float
realFrac frac = read ('0' : frac)

realExp :: [Char] -> Int
realExp (e : '+' : digits) = read digits
realExp (e       : digits) = read digits

int :: Parser [Char]
int = many1 digit

frac :: Parser [Char]
frac = ((:) <$> char '.' <*> many1 digit)

expo :: Parser [Char]
expo =
    (do
            e      <- char 'E'
            sign   <- sign
            digits <- many1 digit
            return (e : sign ++ digits)
        )
        <|> (do
                e      <- char 'e'
                sign   <- sign
                digits <- many1 digit
                return (e : sign ++ digits)
            )
-- <|> ((:) <$> char 'E' <*> ((++) <$> sign <*> many1 digit))
-- <|> ((:) <$> char 'e' <*> ((++) <$> sign <*> many1 digit))

sign :: Parser [Char]
sign = string "" <|> string "-" <|> string "+"

array :: Parser FREDValue
array =
    lexeme $ A <$> ((lexeme (char '[')) *> many atom <* (lexeme (char ']')))

object :: Parser FREDValue
object = lexeme (O <$> ((char '{') *> many pair <* char '}'))

pair :: Parser (String, FREDValue)
pair = do
    skipWs
    key <- name <|> quotedName
    skipWs
    char ':'
    skipWs
    value <- value <* parserTrace "pair value"
    return (key, value)

name :: Parser String
name = lexeme (many1 (oneOf (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'])))

quotedName :: Parser String
quotedName = lexeme stringLiteral

main :: IO ()
main = do
    result <- parseFromFile value "test.fred"
    case result of
        Left  err -> print err
        Right xs  -> print xs
