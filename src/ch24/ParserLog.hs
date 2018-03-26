-- 5. Log file parser --> TODO
module ParserLog where

import Text.Trifecta
import Data.Time
import Control.Applicative
import ParserUtils

skipCommentOrDate :: Parser ()
skipCommentOrDate =
  skipMany (do
    string "#" <|> string "--"
    skipMany (noneOf "\n")
    skipEOL)

parseDate' :: Parser ()
parseDate' = do
  y <- parseFour
  char '-'
  m <- parseTwo
  char '-'
  d <- parseTwo
  skipEOL

-- parseLine :: Parser ([String], Int, Int)
parseLine = do
  h <- parseTwo
  c <- char ':'
  m <- parseTwo
  skipMany space
  act <- manyTill (noneOf "\n") (try (string "--")) --doesn't work!
  skipMany $ noneOf "\n"
  skipEOL
  return (act, h, m)

readTime' :: String -> String -> UTCTime
readTime' f dateString = parseTimeOrError True defaultTimeLocale f dateString

diffTime :: UTCTime -> UTCTime -> Integer
diffTime t1 t2 = floor . toRational $ diffUTCTime t2 t1

-- parseDate :: Parser String
-- parseDate = do
--   between (string "# ") (string "\n") (some letter)


-- p1 :: Parser [([String], Int, Int)]
p1 = skipCommentOrDate >> many parseLine

pr :: IO ()
pr = do
  c <- readFile "data/logfile"
  print $ parseString p1 mempty c

