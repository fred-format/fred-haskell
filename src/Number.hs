module Number
    ( number
    , frac
    )
where

import           Text.Parsec
import           Text.Parsec.String
import           Numeric
import           GenericCombinators
import           FREDValue
import           Data.Char                      ( digitToInt )
import           Data.List                      ( foldl' )

number :: Parser FREDValue
number =
    (N <$> try hex)
        <|> (N <$> try oct)
        <|> (N <$> try binary)
        <|> (N <$> decimal)


hex :: Parser (Either Integer Float)
hex = do
    string "0x"
    firstHexDigit <- hexDigit
    hexDigits     <- many (hexDigit <|> (char '_' *> hexDigit))
    let [(hex, _)] = readHex (firstHexDigit : hexDigits)
    return (Left hex)

oct :: Parser (Either Integer Float)
oct = do
    string "0o"
    firstOctDigit <- octDigit
    octDigits     <- many (octDigit <|> (char '_' *> octDigit))
    let [(oct, _)] = readOct (firstOctDigit : octDigits)
    return (Left oct)

binary :: Parser (Either Integer Float)
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

decimal :: Parser (Either Integer Float)
decimal = (toNumber <$> int <*> (option [] frac) <*> (option [] expo))

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
            
sign :: Parser [Char]
sign = string "" <|> string "-" <|> string "+"
