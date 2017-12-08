module Chapter8 where

import Data.List (intersperse)

-- Intermission: Exercise
applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f . applyTimes (n - 1) f $ b

-- applyTimes 5 (+1) 5
applyFive = applyTimes 4 (+1) $ applyTimes 3 (+1) $ applyTimes 2 (+1) $ applyTimes 1 (+1) $ applyTimes 0 (+1) 0

-- Chapter Exercises

-- Review of types
-- 1. d)
-- 2. b)
-- 3. d)
-- 4. b)

-- Reviewing currying
cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy :: String -> String -> String
flippy = flip cattyConny

appedCatty :: String -> String
appedCatty = cattyConny "woops"

frappe :: String -> String
frappe = flippy "haha"

-- 1. appedCatty "woohoo!" -> "woops mrow woohoo!"
-- 2. frappe "1" -> "1 mrow haha"
-- 3. frappe (appedCatty "2") -> "woops mrow 2 mrow haha"
-- 4. appedCatty (frappe "blue") -> "woops mrow blue mrow haha"
-- 5. cattyConny (frappe "pink") (cattyConny "green" (appedCatty "blue")) -> "pink mrow haha mrow green mrow woops mrow blue"
-- 6. cattyConny (flippy "Pugs" "are") "awesome" -> "are mrow Pugs mrow awesome"

-- Recursion
-- 1.
dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n - d) d (count + 1)
-- dividedBy 15 2 =
--   go 15 2 0
--   go (15 - 2) 2 (0 + 1)
--   go 13 2 1
--   go (13 - 2) 2 (1 + 1)
--   go 11 2 2
--   go (11 - 2) 2 (2 + 1)
--   go 9 2 3
--   go (9 - 2) 2 (3 + 1)
--   go 7 2 4
--   go (7 - 2) 2 (4 + 1)
--   go 5 2 5
--   go (5 - 2) 2 (5 + 1)
--   go 3 2 6
--   go (3 - 2) 2 (6 + 1)
--   go 1 2 7
--   1 < 2 = (7, 1)

-- 2.
mySum :: (Eq a, Num a) => a -> a
mySum n
  | n == 0 = 0
  | otherwise = mySum (n - 1) + n

--3.
myMult :: (Integral a) => a -> a -> a
myMult x y = go x y where
  go x' y'
    | x' == 0 || y' == 0 = 0
    | x' == 1 = y'
    | y' == 1 = x'
    | otherwise = go (x' + x) (y' - 1)

-- Fixing dividedBy
data DividedResult =  Result Integer | DividedByZero deriving Show

-- div as well as divMod work strange with negative numbers; e.g. div 4 (-3) returns -2 not -1
dividedBy' :: Integral a => a -> a -> (DividedResult, a)
dividedBy' num denom = go num denom 0
  where go n d count
          | d == 0 = (DividedByZero, 0)
          | n < 0 && d < 0 = fn (abs n) (abs d)
          | n < 0 || d < 0 = fn' n d
          | n < d && denom < 0 = (Result count, -n)
          | n < d = (Result count, n)
          | otherwise = fn n d
          where fn x y = go (x - y) y (count + 1)
                fn' x y = go (x + y) y (count - 1)

-- TODO: try to implement quotRem

-- McCarthy 91 function
mc91 :: Integral a => a -> a
mc91 x 
  | x > 100 = x - 10
  | otherwise = mc91 . mc91 $ x + 11

-- Numbers into words
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
