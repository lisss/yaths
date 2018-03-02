-- 4. Write a parser for US/Canada phone numbers with varying formats.
module ParserPhone where

import Text.Trifecta
import Control.Applicative
import ParserUtils
  
type NumberingPlanArea = Int
type Exchange = Int
type LineNumber = Int
data PhoneNumber =
  PhoneNumber NumberingPlanArea
  Exchange LineNumber
  deriving (Eq, Show)

parsePhone1 :: Parser PhoneNumber
parsePhone1 = do
  nar <- parseThree
  ex <- parseThree
  ln <- parseFour
  return $ PhoneNumber nar ex ln

parsePhone2 :: Parser PhoneNumber
parsePhone2 = do
  nar <- parseThree
  char '-'
  ex <- parseThree
  char '-'
  ln <- parseFour
  return $ PhoneNumber nar ex ln

parsePhone3 :: Parser PhoneNumber
parsePhone3 = do
  nar <- between (symbol "(") (symbol ") ") parseThree
  ex <- parseThree
  char '-'
  ln <- parseFour
  return $ PhoneNumber nar ex ln

parsePhone4 :: Parser PhoneNumber
parsePhone4 = digit >> char '-' >> parsePhone2

parsePhone :: Parser PhoneNumber
parsePhone =
  (try parsePhone1
    <|> try parsePhone2
    <|> try parsePhone3
    <|> parsePhone4)
  <* eof
