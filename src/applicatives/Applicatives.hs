module Applicatives(
  Identity(Identity)
  ,List(Nil, Cons)
  ,main) where

import Data.List (elemIndex)
import Data.Monoid((<>), Sum)
import Control.Applicative (liftA3)
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import Functors(
  Identity(Identity)
  ,List(Nil, Cons)
  ,Pair(Pair)
  ,Two(Two)
  ,Three(Three)
  ,Three'(Three')
  ,Four(Four)
  ,Four'(Four'))

-- Exercises: Lookups

-- 1.
added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1, 2, 3] [4, 5, 6])

-- 2.
y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer)
tupled = (,) <$> y <*> z

-- 3.
x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]

y1 :: Maybe Int
y1 = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int
max' = max

maxed :: Maybe Int
maxed = max' <$> x <*> y1

xs = [1, 2, 3]
ys = [4, 5, 6]

x2 :: Maybe Integer
x2 = lookup 3 $ zip xs ys

y2 :: Maybe Integer
y2 = lookup 2 $ zip xs ys

summed :: Maybe Integer
summed = (<$>) sum $ (,) <$> x2 <*> y2

-- Exercise: Identity Instance

-- See Identity type from Functors module
instance Applicative Identity where
  pure a = Identity a
  (<*>) (Identity f) (Identity a) = Identity (f a)

instance Eq a => EqProp (Identity a) where (=-=) = eq

-- Exercise: Constant Instance
newtype Constant a b = Constant { getConstant :: a }
  deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant a) (Constant b) = Constant (a <> b)

instance (Eq a, Eq b) => EqProp (Constant a b) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (Constant a b) where
  arbitrary = do
    a <- arbitrary
    return (Constant a)

-- Exercise: Fixer Upper
-- 1. 
xx = const <$> Just "Hello" <*> pure "World" :: Maybe String
-- 2. 
yy = (,,,) <$> Just 90 <*> Just 10 <*> Just "Tierness" <*> (pure [1, 2, 3] :: Maybe [Int])

-- See List from Functors module
fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

instance Monoid (List a) where
  mempty = Nil
  mappend Nil x = x
  mappend (Cons x xs) ys = Cons x (mappend xs ys)
  
concat' :: List (List a) -> List a
concat' = fold mappend Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f as = concat' $ fmap f as

instance Applicative List where
  pure a = Cons a Nil
  (<*>) Nil xs = Nil
  (<*>) (Cons f xs) ys = (flatMap (\x -> Cons (f x) Nil) ys) <> (xs <*> ys)

f = Cons (+1) (Cons (*2) Nil)
v = Cons 1 (Cons 2 Nil)
fv = f <*> v

instance Eq a => EqProp (List a) where (=-=) = eq

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    elements [Nil, Cons a Nil]

-- ZipList Applicative Exercise
take' :: Int -> List a -> List a
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)

newtype ZipList' a = ZipList' (List a) deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
  pure a = ZipList' (Cons a Nil)
  (<*>) (ZipList' f) (ZipList' l) = ZipList' (f <*> l)

-- Exercise: Variations on Either
data Validation e a =
  Failure' e
  | Success' a
  deriving (Eq, Show)

data Errors =
  DividedByZero
  | StackOverflow
  | MooglesChewedWires
  deriving (Eq, Show)

instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

instance Monoid e => Applicative (Validation e) where
  pure = Success'
  (<*>) (Failure' f) (Failure' ff) = Failure' $ f <> ff
  (<*>) (Failure' f) _ = Failure' f
  (<*>) _ (Failure' f) = Failure' f
  (<*>) (Success' f) s = fmap f s

instance (Eq a, Eq b) => EqProp (Validation a b) where (=-=) = eq

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do 
    e <- arbitrary
    a <- arbitrary
    elements [Success' a, Failure' e]

-- Chapter Exercises
-- specialize the types of the methods

-- 1. []
-- pure :: a -> [] a
-- (<*>) :: [] (a -> b) -> [] a -> [] b

-- 2. IO
-- pure :: a -> IO a
-- (<*>) :: IO (a -> b) -> IO a -> IO b

-- 3. (,) a --> TODO
-- pure :: a -> (b -> (a, b))

-- 4. (->) e --> TODO
-- pure :: (e -> a -> e)

-- Write instances for the following datatypes

-- 1. See Pair type from Functors module
instance Applicative Pair where
  pure a = Pair a a
  (<*>) (Pair f g) (Pair a b) = Pair (f a) (g b)

instance Eq a => EqProp (Pair a) where (=-=) = eq

-- 2. See Two type from Functors module
instance Monoid a => Applicative (Two a) where
  pure a = Two mempty a
  (<*>) (Two x f) (Two x' y) = Two (x <> x') (f y)

instance (Eq a, Eq b) => EqProp (Two a b) where (=-=) = eq

-- 3. See Three type from Functors module
instance (Monoid a, Monoid b) => Applicative (Three a b) where
  pure x = Three mempty mempty x
  (<*>) (Three x y z) (Three x' y' z') = Three (x <> x') (y <> y') (z z')

instance (Eq a, Eq b, Eq c) => EqProp (Three a b c) where (=-=) = eq

-- 4. See Three' type from Functors module
instance Monoid a => Applicative (Three' a) where
  pure x = Three' mempty x x
  (<*>) (Three' x y z) (Three' x' y' z') = Three' (x <> x') (y y') (z z')

instance (Eq a, Eq b) => EqProp (Three' a b) where (=-=) = eq

-- 5. See Four type from Functors module
instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
  pure x = Four mempty mempty mempty x
  (<*>) (Four x y z w) (Four x' y' z' w') = Four (x <> x') (y <> y') (z <> z') (w w')

instance (Eq a, Eq b, Eq c, Eq d) => EqProp (Four a b c d) where (=-=) = eq

-- 6. See Four' type from Functors module
instance Monoid a => Applicative (Four' a) where
  pure x = Four' mempty mempty mempty x
  (<*>) (Four' x y z w) (Four' x' y' z' w') = Four' (x <> x') (y <> y') (z <> z') (w w')

instance (Eq a, Eq b) => EqProp (Four' a b) where (=-=) = eq

-- Combinations
stops :: String
stops = "pbtdkg"

vowels :: String
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos [] [] [] = []
combos x y z = liftA3 (\a b c -> (a,b,c)) x y z

--- MAIN ---
main :: IO ()
main = do
  quickBatch $ applicative (Identity ("a", "b", "c") :: Identity (String, String, String))
  quickBatch $ applicative (Constant "a" :: Constant String (String, String, String))
  
  let list = Cons (1 :: Int, 2 :: Int, 3 :: Int) Nil
  quickBatch $ applicative list
  quickBatch $ applicative $ ZipList' list

  quickBatch $ applicative
    (Failure' "err1"
      :: Validation String (String, String, String))
  quickBatch $ applicative $
    Pair (1 :: Int, 1 :: Int, 1 :: Int) (2 :: Int, 2 :: Int, 2 :: Int)

  quickBatch $ applicative $
    Two ("a", "b", "c") (2 :: Int, 2 :: Int, 2 :: Int)

  quickBatch $ applicative $
    Three
      ("a", "b", "c")
      ([1 :: Int], [2 :: Int], [3 :: Int])
      (2 :: Int, 2 :: Int, 2 :: Int)

  quickBatch $ applicative $
    Three'
      ("a", "b", "c")
      (2 :: Int, 2 :: Int, 2 :: Int)
      (2 :: Int, 2 :: Int, 2 :: Int)

  quickBatch $ applicative $
    Four
      ("a", "b", "c")
      ([1 :: Int], [2 :: Int], [3 :: Int])
      ([4 :: Int], [5 :: Int], [6 :: Int])
      (2 :: Int, 2 :: Int, 2 :: Int)

  quickBatch $ applicative $
    Four'
      ("a", "b", "c")
      ("d", "e", "f")
      ("g", "h", "i")
      (2 :: Int, 2 :: Int, 2 :: Int)
