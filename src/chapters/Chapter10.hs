module Chapter10 where

import Data.Time
import Data.Foldable(foldl')

-- foldr and foldl evaluation
list = map show [1..5]
yr = foldr (\x y -> concat ["(",x,"+",y,")"]) "0" list
yl = foldl (\x y -> concat ["(",x,"+",y,")"]) "0" list

-- The relationship between the scans and folds
-- last (scanl f z xs) = foldl f z xs
-- head (scanr f z xs) = foldr f z xs

-- Exercises: Understanding Folds
-- 1. b)
-- 2. foldl (flip (*)) 1 [1..3] -->
-- (3 * (2 * (1 * 1)))
-- 3. c)
-- 4. a)
-- 5.
-- a) foldr (++) [] ["woot", "WOOT", "woot"]
-- b) foldr max 'a' "fear is the little death"
-- c) foldr && True [False, True]
-- d) foldr (||) False [False, True]
-- e) foldl (flip $ (++) . show) "" [1..5] --> explain
-- f) foldr const 0 [1..5]
-- g) foldr const '0' "tacos"
-- h) foldl (flip const) 'a' "burritos"
-- i) foldl (flip const) 0 [1..5]

-- Exercises: Database Processing
data DatabaseItem = DbString String
  | DbNumber Integer
  | DbDate UTCTime
  deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [ DbDate (UTCTime
    (fromGregorian 1911 5 1)
    (secondsToDiffTime 34123))
  , DbNumber 9001
  , DbString "Hello, world!"
  , DbDate (UTCTime
    (fromGregorian 1921 5 1)
    (secondsToDiffTime 34123))
  ]

-- 1. // TODO: rewrite using fold
filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate xs = map getTime (filter isD xs)
  where isD y
          | (DbDate _) <- y = True
          | otherwise = False
        getTime (DbDate x) = x
        getTime _ = undefined

-- 2.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber xs = map getNum (filter isNum xs)
  where isNum y
          | (DbNumber _) <- y = True
          | otherwise = False
        getNum (DbNumber x) = x
        getNum _ = 0

-- 3.
mostRecent :: [DatabaseItem] -> UTCTime
mostRecent xs = foldr max
  (UTCTime
    (fromGregorian 0 0 0)
    (secondsToDiffTime 0)
  )
  (filterDbDate xs)

-- 4.
sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (+) 0 . filterDbNumber

-- 5.
avgDb :: [DatabaseItem] -> Double
avgDb xs = fromIntegral (sumDb xs) / fromIntegral (length (l xs))
  where l y = filterDbNumber y

-- Scans Exercises
fibs :: [Integer]
fibs = 1 : scanl (+) 1 fibs

-- 1.
fibs' :: [Integer]
fibs' = take 20 $ 1 : scanl (+) 1 fibs'

-- 2.
fibs'' :: [Integer]
fibs'' = takeWhile (< 100) $ 1 : scanl (+) 1 fibs''

-- 3.
factorial :: Integer -> Integer
factorial 0 = 1
factorial a = a * factorial (a - 1)

factorial' :: Integer -> Integer
factorial' a = last $ scanl (*) 1 [1 .. a]

-- Chapter Exercises

-- Warm-up and review
-- 1.
stops = "pbtdkg"
vowels = "aeiou"
nouns = ["cat", "ale", "bear"]
verbs = ["meows", "tastes", "mmm"]

-- a).
svs :: [(Char, Char, Char)]
svs = [(a, b, c) | a <- stops, b <- vowels, c <- stops]

-- b).
svs' :: [(Char, Char, Char)]
svs' = filter (\(x', _, _) -> x' == 'p') [(a, b, c) | a <- stops, b <- vowels, c <- stops]

-- c).
svs'' :: [(String, String, String)]
svs'' = [(a, b, c) | a <- nouns, b <- verbs, c <- nouns]

-- 2.
seekritFunc x =
  div (sum (map length (words x)))
  (length (words x))
-- returns a result of division of sum of lengths of a single word from list of words and number of words

-- 3.
seekritFunc' x =
  fromIntegral (sum (map length (words x))) / fromIntegral (length (words x))

-- Rewriting functions using folds
-- myAnd :: [Bool] -> Bool
-- myAnd [] = True
-- myAnd (x:xs) =
--   if x == False
--   then False
--   else myAnd xs
-- -- direct recursion, using (&&)
-- myAnd :: [Bool] -> Bool
-- myAnd [] = True
-- myAnd (x:xs) = x && myAnd xs
-- -- fold, not point-free
-- -- in the folding function
-- myAnd :: [Bool] -> Bool
-- myAnd = foldr
--   (\a b ->
--   if a == False
--   then False
--   else b) True
-- myAnd :: [Bool] -> Bool
-- myAnd = foldr (&&) True

-- 1.
myOr :: [Bool] -> Bool
myOr = foldr (||) False

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny f = foldr (\x y -> f x || y) False

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem x = foldr (\x' y -> x' == x || y) False

-- 4.
myReverse :: [a] -> [a]
myReverse x = foldr (\x' xs -> xs ++ [x']) [] x

-- 5.
myMap :: (a -> b) -> [a] -> [b]
myMap f x = foldr (\x' xs' -> f x' : xs') [] x

-- 6.
myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter f x = foldr isMatch [] x
--   where isMatch x' xs' = if f x' then x' : xs' else xs'
myFilter f = foldr (\x -> if f x then (x :) else id) [] -- found on the Internet, more sexy

-- 7.
squish :: [[a]] -> [a]
squish = foldr (++) []

-- 8.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f x = foldr (\x' xs' -> f x' ++ xs') [] x

-- 9.
squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

-- TODO: Fix non-exhaustive pattern match
-- TODO: why foldr doesn't work???
-- 10.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl' (\a b -> if f a b == GT then a else b) x xs

-- 11.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl' (\a b -> if f a b == LT then a else b) x xs
