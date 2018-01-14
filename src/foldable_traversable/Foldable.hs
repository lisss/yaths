module Foldable where

import Data.Monoid

-- Exercises: Library Functions

-- 1.
sum' :: (Foldable t, Num a) => t a -> a
sum' = foldr (+) 0

-- 2.
product' :: (Foldable t, Num a) => t a -> a
product' = foldr (*) 1

-- 3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' a t = getAny $ foldMap (\x -> Any (x == a)) t

-- 4. TODO: simplify?
minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = foldr go Nothing
  where
    go x Nothing = Just x
    go x (Just y) = Just (min x y)

-- 5.
maximum' :: (Foldable t, Ord a) => t a -> Maybe a
maximum' = foldr go Nothing
  where
    go x Nothing = Just x
    go x (Just y) = Just (max x y)

-- 6.
null' :: (Foldable t) => t a -> Bool
null' = foldr (\_ _ -> False) True

-- 7.
length' :: (Foldable t) => t a -> Int
length' = foldr (\_ x -> x + 1) 0

-- 8.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x xs -> x:xs) []

-- 9. Hint: use foldMap.
-- | Combine the elements
-- of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = mconcat . toList'

-- 10. Define foldMap in terms of foldr.
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr (\x _ -> f x) mempty

-- Chapter Exercises
-- 1.
newtype Constant a b = Constant b deriving (Eq, Show)

instance Foldable (Constant a) where
  foldr f z (Constant x) = f x z
  foldl f z (Constant x) = f z x
  foldMap f (Constant x) = f x

-- 2.
data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two _ b) = f b z
  foldl f z (Two _ b) = f z b
  foldMap f (Two _ b) = f b

-- 3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ c) = f c z
  foldl f z (Three _ _ c) = f z c
  foldMap f (Three _ _ c) = f c

-- 4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' _ _ b) = f b z
  foldl f z (Three' _ _ b) = f z b
  foldMap f (Three' _ _ b) = f b

-- 5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' _ _ _ b) = f b z
  foldl f z (Four' _ _ _ b) = f z b
  foldMap f (Four' _ _ _ b) = f b

-- 6.
filterF :: (Applicative f, Foldable t, Monoid (f a))
  => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

main = print ""