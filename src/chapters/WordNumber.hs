module WordNumber where

import Data.List (intersperse)

digitToWord :: Integer -> String
digitToWord n
  | n == 0 = "zero"
  | n == 1 = "one"
  | n == 2 = "two"
  | n == 3 = "three"
  | n == 4 = "four"
  | n == 5 = "five"
  | n == 6 = "six"
  | n == 7 = "seven"
  | n == 8 = "eight"
  | n == 9 = "nine"

digits :: Integer -> [Integer]
digits n = go n [] where
  go x y
    | div x 10 == 0 = x:y
    | otherwise = go (div x 10) (mod x 10 : y)

wordNumber :: Integer -> String
wordNumber = concat . intersperse "-" . map digitToWord . digits
