module ParserUtils where

import Text.Trifecta

skipEOL :: Parser ()
skipEOL = skipMany (oneOf "\n")

parseTwo :: Parser Int
parseTwo = read <$> count 2 digit <?> "parseTwo"

parseThree :: Parser Int
parseThree = read <$> count 3 digit <?> "parseThree"

parseFour :: Parser Int
parseFour = read <$> count 4 digit <?> "parseFour"
