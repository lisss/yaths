-- 1. SemVer parser
module ParserSemVer where

import Text.Trifecta
import Control.Applicative

data NumOrStr = NOSS String | NOSI Integer deriving (Show, Eq, Ord)
type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumOrStr]
type Metadata = [NumOrStr]
-- TODO: write Ord instance
data SemVer = SemVer Major Minor Patch Release Metadata deriving (Show, Eq, Ord)

parseNos' :: Parser NumOrStr
parseNos' =
   NOSS <$> some letter
   <|> NOSI <$> integer

-- TODO: ineficcient to append to the end of the list in such a way
-- any better options (e.g. use fold)?
parseReleaseOrMeta :: Char -> Parser [NumOrStr]
parseReleaseOrMeta separator = do
  sep <- optional $ char separator
  case sep of
    Nothing -> return []
    _ -> go []
    where go xs = do
              a <- parseNos'
              c <- optional $ char '.'
              case c of
                Nothing -> return $ xs ++ [a]
                _ -> go (xs ++ [a])
  
parseSemVer :: Parser SemVer
parseSemVer = do
  maj <- decimal -- use natural? check it
  char '.'
  min <- decimal
  char '.'
  patch <- decimal
  rel <- parseReleaseOrMeta '-'
  meta <- parseReleaseOrMeta '+'
  return $ SemVer maj min patch rel meta
