module Traversable (Optional(Nada,Only)) where

import Data.ByteString.Lazy hiding (map)
import Network.Wreq
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

-- Example using wreq lib

urls :: [String]
urls = [ "http://httpbin.org/ip"
  , "http://httpbin.org/bytes/5"
  ]

mappingGet :: [IO (Response ByteString)]
mappingGet = map get urls

traversedUrls :: IO [Response ByteString]
traversedUrls = traverse get urls

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