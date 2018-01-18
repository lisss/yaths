module Traversable (
  Optional(Nada,Only)
  ,S(S)
  ,Tree(Leaf,Node)
) where

-- import Data.ByteString.Lazy hiding (map)
-- import Network.Wreq
import Foldable(
  Identity(Identity)
  ,Constant(Constant)
  ,List(Nil,Cons)
  ,Two(Two)
  ,Three(Three)
  ,Three'(Three')
  ,Four'(Four')
  ,Four''(Four'')
  ,Optional(Nada, Only)
  )
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Data.Monoid((<>))

-- Example using wreq lib

-- urls :: [String]
-- urls = [ "http://httpbin.org/ip"
--   , "http://httpbin.org/bytes/5"
--   ]

-- mappingGet :: [IO (Response ByteString)]
-- mappingGet = map get urls

-- traversedUrls :: IO [Response ByteString]
-- traversedUrls = traverse get urls

-- Chapter Exercises

-- Traversable instances

-- See Identity type from Foldable module
instance Traversable Identity where
  traverse f (Identity a) = Identity <$> f a

-- See Constant type from Foldable module
instance Traversable (Constant a) where
  traverse f (Constant a) = pure $ Constant a

-- See Optional type from Foldable module
instance Traversable Optional where
  traverse f Nada = pure Nada
  traverse f (Only a) = Only <$> f a

instance Arbitrary a => Arbitrary (Optional a) where
  arbitrary = do
    a <- arbitrary
    elements [Nada, Only a]

instance Eq a => EqProp (Optional a) where (=-=) = eq

-- See List type from Foldable module
instance Traversable List where
  traverse f Nil = pure Nil
  traverse f (Cons a lst) = Cons <$> f a <*> traverse f lst

-- See Three type from Foldable module
instance Traversable (Three a b) where
  traverse f (Three a b c) = Three a b <$> f c

-- Pair. See Two type from Foldable module
instance Traversable (Two a) where
  traverse f (Two a b) = Two a <$> f b

-- Big. See Three' type from Foldable module
instance Traversable (Three' a) where
  traverse f (Three' a b c) = Three' a <$> f b <*> f c

-- Bigger. See Four'' type from Foldable module
instance Traversable (Four'' a) where
  traverse f (Four'' a b c d) = Four'' a <$> f b <*> f c <*> f d

-- S
data S n a = S (n a) a deriving (Eq, Show)

instance (Functor n, Arbitrary (n a), Arbitrary a)
  => Arbitrary (S n a) where
  arbitrary = S <$> arbitrary <*> arbitrary

-- The EqProp instance from the book fails Functor tests for some reason
-- TODO: identify why
-- instance (Applicative n, Testable (n Property), EqProp a)
--   => EqProp (S n a) where
--     (S x y) =-= (S p q) = property ((=-=) <$> x <*> p) .&. (y =-= q)

instance (Eq (n a), Eq a) => EqProp (S n a) where (=-=) = eq

instance Functor n => Functor (S n) where
  fmap f (S na a) = S (fmap f na) (f a)

instance Foldable n => Foldable (S n) where
  foldr f z (S na a) = foldr f (f a z) na
  foldMap f (S na a) = foldMap f na <> f a

instance Traversable n => Traversable (S n) where
  traverse f (S na a) = S <$> traverse f na <*> f a

-- Tree
data Tree a =
  Empty
  | Leaf a
  | Node (Tree a) a (Tree a)
  deriving (Eq, Show)

instance Functor Tree where
  fmap f (Leaf a) = Leaf (f a)
  fmap f (Node left a right) = Node (fmap f left) (f a) (fmap f right)

instance Foldable Tree where
  foldMap f (Leaf a) = f a
  foldMap f (Node left a right) = foldMap f left <> f a <> foldMap f right
  foldr f z (Leaf x) = f x z
  foldr f z (Node left a right) = foldr f (f a (foldr f z right)) left

instance Traversable Tree where
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Node left a right) = Node <$> traverse f left <*> f a <*> traverse f right

instance Eq a => EqProp (Tree a) where (=-=) = eq

instance Arbitrary a => Arbitrary (Tree a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    frequency [
      (1, return (Leaf a))
      ,(1, return (Node (Leaf b) a (Leaf c)))
      ] 
