{-# LANGUAGE QuasiQuotes #-}
module ParserExamples where

import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ
import Data.Ratio((%))
import ParserUtils

stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

one' :: Parser Char
one' = one >> stop

oneTwo :: Parser Char
oneTwo = char '1' >> char '2'

oneTwo' :: Parser Char
oneTwo' = oneTwo >> stop

-- TODO: can we make it generic somehow?
testParse :: Parser Char -> IO ()
testParse p =
  print $ parseString p mempty "123"

-- Exercises: Parsing Practice
-- 1.
oneEOF :: IO ()
oneEOF = print $ parseString (one >> eof) mempty "123" 

oneTwoEOF :: IO ()
oneTwoEOF = print $ parseString (oneTwo >> eof) mempty "123" 

-- 2. TODO: learn more about <*
-- try choise
eitherOfThree :: Parser String
eitherOfThree = (string "123" <|> string "12" <|> string "1") <* eof

-- TODO: learn more about stop
eitherOfThreeStop :: Parser String
eitherOfThreeStop = (string "123" <|> string "12" <|> string "1") <* stop

-- 3. TODO: myString :: String -> Parser String --> check 
eitherOfThreeChar :: Parser Char
eitherOfThreeChar = (char 'a' <|> char 'b' <|> char 'c') <* eof

-- Exercise: Unit of Success (integer <* eof)
parseInt :: Parser Integer
parseInt = do
  a <- integer
  eof
  return a

-- Example: Alternative
type NumberOrString = Either Integer String

a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos =
  skipEOL
  >>
  (Left <$> integer)
  <|> (Right <$> some letter)

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]

-- Exercise: Try Try
-- TODO: try to make it process data better:
-- don't run parseDecimal if parseRatio failed in case '/' was found
badFraction = "1/0"
alsoBad = "10"
shouldWork = "1/2"
shouldAlsoWork = "2/1"

parseRatio :: Parser Rational
parseRatio = do
  numerator <- decimal
  char '/'
  denominator <- decimal
  case denominator of
    0 -> fail "Denominator cannot be zero"
    _ -> return (numerator % denominator)

type FractOrDecimal = Either Rational Double

parseDecimal :: Parser Double
parseDecimal = do
  a <- decimal
  c <- optional $ char '.'
  case c of
    Nothing ->
      eof >>= \_ ->
      return $ fromInteger a
    _ -> do
      b <- decimal
      eof
      return (fromInteger a + (fromInteger b / (10 ^ length (show b))))

parseDecOrFract :: Parser FractOrDecimal
parseDecOrFract =
  (Left <$> try parseRatio)
  <|> (Right <$> try parseDecimal)
