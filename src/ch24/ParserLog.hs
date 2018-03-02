-- 5. Log file parser --> TODO
module ParserLog where

import Text.Trifecta
import Data.Time
import ParserUtils

readFile1 :: IO ()
readFile1 = do
  f <- readFile "data/logfile"
  print f

skipComment :: Parser ()
skipComment =
  skipMany (do
    _ <- string "--"
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

parseTime' :: Parser (String, Int, Int)
parseTime' = do
  h <- parseTwo
  c <- char ':'
  m <- parseTwo
  space
  l <- many letter
  s <- many space
  -- ln <- parseLine
  return (l, h, m)

parseLine :: Parser String
parseLine = do
  l <- some letter
  s <- optional space
  case s of
    Nothing -> do
      char '\n'
      return l
    _ -> parseLine

readTime' :: String -> String -> UTCTime
readTime' f dateString = parseTimeOrError True defaultTimeLocale f dateString

diffTime :: UTCTime -> UTCTime -> Integer
diffTime t1 t2 = floor . toRational $ diffUTCTime t2 t1

-- parseDate :: Parser String
-- parseDate = do
--   between (string "# ") (string "\n") (some letter)

skipStartDate :: Parser ()
skipStartDate =
  skipMany (do
    _ <- string "#"
    skipMany (noneOf "\n")
    skipEOL)

p :: Parser [(String, Int, Int)]
p = skipComment >> skipStartDate >> many parseTime'

pr :: IO ()
pr = do
  c <- readFile "data/logfile"
  print $ parseString p mempty c

