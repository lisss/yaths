module Chapter12 where

import Chapter9(myWords)
import Chapter10(vowels)
import Chapter11(BinaryTree(Node, Leaf))

-- Chapter Exercises
-- 1. id :: a -> a --> *
-- 2. r :: a -> f a --> *; * -> *

-- String processing
-- TODO: rewrite!!!!!
separate :: String -> [String]
separate x = go x
  where go x'
          | null x' = []
          | otherwise = takeWhile (/= ' ') x' : " " : go (dropWhile (== ' ') . dropWhile (/= ' ') $ x')
          

notThe :: String -> Maybe String
notThe x
  | x == "the" = Nothing
  | otherwise = Just x

replaceThe :: String -> String
replaceThe x
  | [] <- x = []
  | otherwise = repl (separate x ) where
    repl [] = []
    repl (x':xs)
      | null x' = []
      | otherwise = repl' x' ++ repl xs

repl' :: String -> String
repl' x = case notThe x of
  Nothing -> "a"
  Just x' -> x'

-- repl'' :: String -> String
-- repl'' x = fromMaybe "a" (notThe x)

-- 2.
countTheBeforeVowel :: String -> Integer
countTheBeforeVowel = getV . myWords where
  getV :: [String] -> Integer
  getV [] = 0
  getV [_] = 0
  getV w = go w 0 where
    go (x':a':xs') count
      | null a' = 0
      | null xs' = if isMatch x' a' then 1 else 0
      | isMatch x' a' = go (a':xs') count + 1
      | otherwise = go (a':xs') count
      where
        isMatch w' n = case notThe w' of
          Nothing -> head n `elem` vowels
          Just _ -> False

-- 3.
countVowels :: String -> Integer
countVowels x = foldr (\x' y -> if x' `elem` vowels then y + 1 else y) 0 x

-- Validate the word
newtype Word' =
  Word' String
  deriving (Eq, Show)
  
mkWord :: String -> Maybe Word'
mkWord x
  | null x = Nothing
  | countVowels x > countConsonants x = Nothing
  | otherwise = Just (Word' x)

countConsonants :: String -> Integer
countConsonants x = foldr (\x' y -> if x' `notElem` vowels then y + 1 else y) 0 x

-- It’s only Natural
data Nat = Zero | Succ Nat deriving (Eq, Show)
  -- >>> natToInteger Zero
-- 0
-- >>> natToInteger (Succ Zero)
-- 1
-- >>> natToInteger (Succ (Succ Zero))
-- 2
natToInteger :: Nat -> Integer
natToInteger x = case x of
  Zero -> 0
  Succ x -> natToInteger x + 1
-- >>> integerToNat 0
-- Just Zero
-- >>> integerToNat 1
-- Just (Succ Zero)
-- >>> integerToNat 2
-- Just (Succ (Succ Zero))
-- >>> integerToNat (-1)
-- Nothing
integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing  
  | x == 0 = Just (Succ Zero)
  | otherwise = Just (res x) where
    res 0 = Zero
    res x' = Succ (res (x' - 1))

-- Small library for Maybe
-- 1.
isJust :: Maybe a -> Bool
isJust Nothing = False
isJust _ = True

isNothing :: Maybe a -> Bool
isNothing Nothing = True
isNothing _ = False

-- 2.
mayybee :: b -> (a -> b) -> Maybe a -> b
mayybee x _ Nothing = x
mayybee _ f (Just x) = f x

-- 3.
fromMaybe :: a -> Maybe a -> a
fromMaybe a Nothing = a
fromMaybe _ (Just x) = x

-- 4.
listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:_) = Just x

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just x) = [x]

-- 5.
fromJust :: Maybe a -> a
fromJust (Just x) = x

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (x:xs)
  | isJust x = fromJust x : catMaybes xs
  | otherwise = catMaybes xs

-- catMaybes :: [Maybe a] -> [a]
-- catMaybes x = [m | Just m <- x ]

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = Just . catMaybes

-- Small library for Either
-- 1.
lefts' :: [Either a b] -> [a]
lefts' = foldr (\x' acc -> case x' of
  Left a -> a : acc
  Right _ -> acc
  ) []

-- 2.
rights' :: [Either a b] -> [b]
rights' = foldr (\x' acc -> case x' of
  Left _ -> acc
  Right b -> b : acc
  ) []

-- 3.
partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr (either left right) ([],[])
  where
    left a (l, r) = (a:l, r)
    right a (l, r) = (l, a:r)

-- 4.
eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f x = case x of
  Left _ -> Nothing
  Right b -> Just (f b)

-- 5.
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' fl _ (Left a) = fl a
either' _ fr (Right b) = fr b

-- 6. 
eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\_ -> Nothing) (\b -> Just (f b)) x

-- Unfolds
-- Write your own iterate and unfoldr
-- 1.
myIterate :: (a -> a) -> a -> [a]
myIterate f x = go x [] where
  go x' xs = x' : go (f x') (tail xs)

-- 2.
myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = go x [] where
  go x' xs = case f x' of
    Nothing -> go x' (tail xs)
    Just (a, b) -> a : go b (tail xs)

-- 3.
betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\b -> Just (b, f b)) x

-- Finally something other than a list!
-- 1.
unfold :: (a -> Maybe (a, b, a)) -> a -> BinaryTree b
unfold f x = go x Leaf where
  go x' tree = case f x' of
    Nothing -> Leaf
    Just (a, b, c) -> Node (go a tree) b (go c tree)

-- 2.
treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfold (\t ->
    if t < n
    then Just (t + 1, t, t + 1)
    else Nothing) 0
