module Number
    ( number
    , frac
    )
where

import           Text.Parsec
import           Text.Parsec.String
import           GenericCombinators
import           FREDValue

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
