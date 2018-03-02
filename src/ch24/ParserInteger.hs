module ParserInteger where
  
import Text.Trifecta
import Data.Foldable(foldl')
import Data.Char

parseDigit :: Parser Char
parseDigit = oneOf ['0'..'9'] <?> "parseDigit"

-- 2. Parser for positive integers
base10Integer :: Parser Integer
base10Integer = do
  ints <- some $ (toInteger . digitToInt) <$> parseDigit <?> "integer"
  return $ foldl' (\acc x -> acc * 10 + x) 0 ints
    
-- 3. Also handle negative nums
base10Integer' :: Parser Integer
base10Integer' = do
  c <- optional $ char '-'
  case c of
    Nothing -> base10Integer
    _ -> negate <$> base10Integer
