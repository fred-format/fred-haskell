module Fred.Parser.Number
    ( number
    , frac
    )
where

import           Text.Parsec
import           Text.Parsec.String
import           Text.Parsec.Char               ( digit )
import           Numeric
import           Fred.Parser.String
import           Fred.Value                     ( FredAtom(..) )
import           Data.Char                      ( digitToInt )
import           Data.List                      ( foldl' )
number :: Parser FredAtom
number =
    (N <$> try hex)
        <|> (N <$> try oct)
        <|> (N <$> try binary)
        <|> (N <$> decimal)


hex :: Parser (Either Integer Double)
hex = do
    string "0x"
    firstHexDigit <- hexDigit
    hexDigits     <- many (hexDigit <|> (char '_' *> hexDigit))
    let [(hex, _)] = readHex (firstHexDigit : hexDigits)
    return (Left hex)

oct :: Parser (Either Integer Double)
oct = do
    string "0o"
    firstOctDigit <- octDigit
    octDigits     <- many (octDigit <|> (char '_' *> octDigit))
    let [(oct, _)] = readOct (firstOctDigit : octDigits)
    return (Left oct)

binary :: Parser (Either Integer Double)
binary = do
    string "0b"
    firstBinDigit <- binDigit
    binDigits     <- many (binDigit <|> (char '_' *> binDigit))
    let bin = toDec (firstBinDigit : binDigits)
    return (Left bin)

binDigit :: Parser Char
binDigit = char '0' <|> char '1'

toDec :: String -> Integer
toDec = foldl' (\acc x -> acc * 2 + toInteger (digitToInt x)) 0

decimal :: Parser (Either Integer Double)
decimal = (toNumber <$> int <*> (option [] frac) <*> (option [] expo))

toNumber :: [Char] -> [Char] -> [Char] -> Either Integer Double
toNumber int []   []                 = Left (read int)
toNumber int frac []                 = Right ((read int) + (readFrac frac))
toNumber int []   (e : '+' : digits) = Left ((read int) * 10 ^ (read digits))
toNumber int []   (e : '-' : digits) = Right ((read int) * 10 ^^ (read digits))
toNumber int frac (e : digits) =
    Right (  ( read int + readFrac frac ) * 10 ^^ read digits ) 

readFrac :: [Char] -> Double
readFrac frac = read ("0" ++ frac)

realExp :: [Char] -> Int
realExp (e : '+' : digits) = read digits
realExp (e       : digits) = read digits

int :: Parser [Char]
int = do
    firstDigit <- digit
    digits     <- many (digit <|> (char '_' *> digit))
    let r = (firstDigit : digits)
    return r

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
                sign   <- option [] sign
                digits <- many1 digit
                return (e : sign ++ digits)
            )

sign :: Parser [Char]
sign = string "-" <|> string "+"
