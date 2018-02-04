module Monads where

import Control.Monad (join)
import Data.Monoid((<>))
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Functors(Sum(First, Second))
import Applicatives(List(Nil, Cons), Identity(Identity))

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m
-- bind = flip (>>=)

-- Short Exercise: Either Monad
-- See Sum type from Functors module

instance Applicative (Sum a) where
  pure = Second
  (<*>) (First a) _ = First a
  (<*>) _ (First a) = First a
  (<*>) (Second f) (Second a) = Second $ f a

instance Monad (Sum a) where
  return = pure
  (>>=) (First a) _ = First a
  (>>=) (Second b) f = f b

instance (Eq a, Eq b) => EqProp (Sum a b) where (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Sum e a) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    elements [First a, Second b]

-- Chapter Exercises
-- 1.
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ _ = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  (<*>) _ _ = NopeDotJpg

instance Monad Nope where
  (>>=) _ _ = NopeDotJpg

instance Eq a => EqProp (Nope a) where (=-=) = eq
instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

-- 2.
data PhhhbbtttEither b a = Left' a | Right' b deriving (Eq, Show)

instance Functor (PhhhbbtttEither b) where
  fmap f (Left' a) = Left' (f a)
  fmap _ (Right' b) = Right' b

instance Applicative (PhhhbbtttEither b) where
  pure = Left'
  (<*>) (Right' a) _ = Right' a
  (<*>) _ (Right' a) = Right' a
  (<*>) (Left' f) (Left' a) = Left' $ f a

instance Monad (PhhhbbtttEither b) where
  (>>=) (Right' a) _ = Right' a
  (>>=) (Left' b) f = f b

instance (Eq a, Eq b) => EqProp (PhhhbbtttEither b a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhhhbbtttEither b a) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    elements [Left' a, Right' b]

-- 3. See Identity type from Applicatives module
instance Monad Identity where
  (>>=) (Identity a) f = f a

-- 4. See List type from Applicatives module
instance Monad List where
  (>>=) Nil _ = Nil
  (>>=) (Cons x xs) f = f x <> (xs >>= f)

-- Write the following functions using the methods provided by Monad and Functor
-- 1.
j :: Monad m => m (m a) -> m a
j a = join a

-- 2.
l1 :: Monad m => (a -> b) -> m a -> m b
l1 f a = fmap f a

-- 3.
l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l2 f a b = a >>= \a' -> b >>= \b' -> return (f a' b')

-- 4.
a :: Monad m => m a -> m (a -> b) -> m b
a x y = y <*> x

-- 5.
meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh (x:xs) f = sequence $ f x : fmap f xs

-- 6.
flipType :: (Monad m) => [m a] -> m [a]
flipType = flip meh id

main :: IO ()
main = do
  quickBatch $ monad (First "aa" :: Sum String (String, String, String))
  quickBatch $ monad (Second ("a", "b", "c") :: Sum String (String, String, String))
  quickBatch $ monad (NopeDotJpg :: Nope (String, String, String))
  quickBatch $ monad
    (Left' ("a", "b", "c")
      :: PhhhbbtttEither (String, String, String) (String, String, String))
  quickBatch $ monad (Identity ("a", "b", "c") :: Identity (String, String, String))
  quickBatch $ monad (Cons (1, 2, 3) Nil :: List (Int, Int, Int))
