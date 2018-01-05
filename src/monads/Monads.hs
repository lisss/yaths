-- <<< TODO >>> Separate out all the repeating types (Identity, Pair etc.)
module Monads where

import Control.Monad (join)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

bind :: Monad m => (a -> m b) -> m a -> m b
bind f m = join $ fmap f m
-- bind = flip (>>=)

-- Short Exercise: Either Monad

data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

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

-- 3.
newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Monad Identity where
  (>>=) (Identity a) f = f a

instance Eq a => EqProp (Identity a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- 4.
data List a = Nil
  | Cons a (List a)

main :: IO ()
main = do
  quickBatch $ monad (First "aa" :: Sum String (String, String, String))
  quickBatch $ monad (Second ("a", "b", "c") :: Sum String (String, String, String))
  quickBatch $ monad (NopeDotJpg :: Nope (String, String, String))
  quickBatch $ monad
    (Left' ("a", "b", "c")
      :: PhhhbbtttEither (String, String, String) (String, String, String))
  quickBatch $ monad (Identity ("a", "b", "c") :: Identity (String, String, String))