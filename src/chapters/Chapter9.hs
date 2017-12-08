module Chapter9 where

import Data.Char
import Data.Bool(bool)

-- Exercise: EnumFromTo
eft :: (Enum a, Ord a) => a -> a -> [a]
eft x y
  | x > y = []
  | otherwise = x : eft (succ x) y

eftBool :: Bool -> Bool -> [Bool]
eftBool = eft

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

-- Exercises: Thy Fearful Symmetry
-- 1.
myWords :: String -> [String]
myWords x = separate x ' '

-- 2.
firstSen = "Tyger Tyger, burning bright\n"
secondSen = "In the forests of the night\n"
thirdSen = "What immortal hand or eye\n"
fourthSen = "Could frame thy fearful symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

myLines :: String -> [String]
myLines x = separate x '\n'

shouldEqual =
  [ "Tyger Tyger, burning bright"
  , "In the forests of the night"
  , "What immortal hand or eye"
  , "Could frame thy fearful symmetry?"
  ]

main :: IO ()
main =
  print $
  "Are they equal? "
  ++ show (myLines sentences == shouldEqual)

-- 3.
separate :: String -> Char -> [String]
separate x y = go x
  where go x
          | null x = []
          | otherwise = takeWhile (/= y) x : go (dropWhile (== y) . dropWhile (/= y) $ x)

-- Exercises: Comprehend Thy Lists --> played in REPL

-- Exercises: Square Cube
mySqr = [x^2 | x <- [1..10]]
myCube = [y^3 | y <- [1..5]]

tpl = [(x, y) | x <- mySqr, y <- myCube]
tplUnder50 = [(x, y) | x <- mySqr, y <- myCube, x < 50, y < 50]
tplOdd = [(x, y) | x <- mySqr, y <- myCube, odd x, odd y]

-- // TODO: interesting observations:
-- λ> let x = [1,2,3]
-- λ> let y = "liss"
-- λ> :sprint x
-- x = _
-- λ> :sprint y
-- y = _
-- λ> take 1 x
-- [1]
-- λ> take 1 y
-- "l"
-- λ> :sprint x
-- x = _
-- λ> :sprint y
-- y = 'l' : _

-- Exercises: Bottom Madness

-- Will it blow up?
-- 1. [x^y | x <- [1..5], y <- [2, undefined]] --> True
-- 2. take 1 $ [x^y | x <- [1..5], y <- [2, undefined]] --> False
-- 3. sum [1, undefined, 3] --> True
-- 4. length [1, 2, undefined] --> False
-- 5. length $ [1, 2, 3] ++ undefined --> True
-- 6. take 1 $ filter even [1, 2, 3, undefined] --> False
-- 7. take 1 $ filter even [1, 3, undefined] --> True
-- 8. take 1 $ filter odd [1, 3, undefined] --> False
-- 9. take 2 $ filter odd [1, 3, undefined] --> False
-- 10. take 3 $ filter odd [1, 3, undefined] --> True

-- Intermission: Is it in normal form? TODO!!!!! re-read it!!!
-- 1. [1, 2, 3, 4, 5] --> NF
-- 2. 1 : 2 : 3 : 4 : _ --> WHNF
-- 3. enumFromTo 1 10 --> WHNF
-- 4. length [1, 2, 3, 4, 5] --> NF
-- 5. sum (enumFromTo 1 10) --> NF
-- 6. ['a'..'m'] ++ ['n'..'z'] --> WHNF
-- 7. (_, 'b') --> WHNF

-- Exercises: More Bottoms
-- 1. take 1 $ map (+1) [undefined, 2, 3] --> bottom
-- 2. take 1 $ map (+1) [1, undefined, 3] --> [2]
-- 3. take 2 $ map (+1) [1, undefined, 3] --> bottom
-- 4. itIsMystery xs = map (\x -> elem x "aeiou") xs -->
  -- takes a string and returns a list of bools indicating if a particular char from the string
  -- is in the list of chars from [a,e,i,o,u]
-- 5.
-- a) map (^2) [1..10] --> [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]
-- b) map minimum [[1..10], [10..20], [20..30]] --> [1, 10, 20]
-- c) map sum [[1..5], [1..5], [1..5]] --> [15, 15, 15]
-- 6.
mapBool :: (Eq a, Num a, Enum a) => [a]
mapBool = map (\x -> bool x (-x) (x == 3)) [1..10]

-- Exercises: Filtering
-- 1.
mult3 = [x | x <- [1..30], x `mod` 3 == 0]
-- 2.
lengthMult3 = length mult3
-- 3.
noArts :: String -> [String]
noArts xs = [x | x <- words xs, x /= "the" && x /= "a" && x /= "an" ]

-- Zipping exercises
-- 1.
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

-- 2.
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ _ [] = []
zipWith' _ [] _ = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- 3.
zip'' :: [a] -> [b] -> [(a, b)]
zip'' = zipWith' (,)

--  Chapter Exercises
-- Data.Char
--1. isUpper :: Char -> Bool; toUpper :: Char -> Char
-- 2.
allUppers :: [Char] -> [Char]
allUppers = filter isUpper

capitalize :: [Char] -> [Char]
capitalize (x:xs) = toUpper x : xs

capitalize' :: [Char] -> [Char]
capitalize' [] = []
capitalize' (x:xs) = toUpper x : capitalize' xs

headCap :: [Char] -> Char
headCap = toUpper . head

-- Ciphers --> see module Cipher

-- Writing your own standard functions
-- 1.
myOr :: [Bool] -> Bool
myOr [] = False
myOr (x:xs) = x || myOr xs

-- 2.
myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = f x || myAny f xs

-- 3.
myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = x == y || myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x = myAny (== x)

-- 4.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ x : []

-- 5.
squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

-- 6.
squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = f x ++ squishMap f xs

-- 7. // TODO - why?
squishAgain :: [[a]] -> [a]
squishAgain [] = []
squishAgain (x:xs) = squishMap (\x -> [x]) x ++ squishAgain xs

-- 8.
myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f x = mySmthBy f x GT

-- 9.
myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f x = mySmthBy f x LT

-- 10.
myMaximum :: (Ord a) => [a] -> a
myMaximum = myMaximumBy compare

myMinimum :: (Ord a) => [a] -> a
myMinimum = myMinimumBy compare


mySmthBy :: (a -> a -> Ordering) -> [a] -> Ordering -> a
mySmthBy _ [] _ = undefined -- what should be the base case here?
mySmthBy _ [x] _ = x
mySmthBy f (x:y:xs) r
    | f x y == r = mySmthBy f (x:xs) r
    | otherwise = mySmthBy f (y:xs) r
