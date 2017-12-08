{-# LANGUAGE DeriveGeneric #-}

module Generators where

import Test.QuickCheck hiding (Arbitrary(Maybe))
import Test.QuickCheck.Gen (oneof)
import GHC.Generics
  
-- Arbitrary instances
-- Babby’s First Arbitrary
-- TODO: move to separate module
data Trivial =
  Trivial
  deriving (Eq, Show)

trivialGen :: Gen Trivial
trivialGen =
  return Trivial

instance Arbitrary Trivial where
arbitrary = trivialGen

main :: IO ()
main = sample trivialGen

-- Identity Crisis
data Identity a =
  Identity a
    deriving (Eq, Show)
    
identityGen :: Arbitrary a => Gen (Identity a)
identityGen = do
  a <- Test.QuickCheck.arbitrary
  return (Identity a)

instance Arbitrary a =>
  Arbitrary (Identity a) where
  arbitrary = identityGen

identityGenInt :: Gen (Identity Int)
identityGenInt = identityGen

-- Arbitrary Products
data Pair a b =
  Pair a b
  deriving (Eq, Show)

pairGen :: (Arbitrary a,
  Arbitrary b) =>
  Gen (Pair a b)
pairGen = do
  a <- Test.QuickCheck.arbitrary
  b <- Test.QuickCheck.arbitrary
  return (Pair a b)

instance (Arbitrary a,
  Arbitrary b) =>
  Arbitrary (Pair a b) where
  arbitrary = pairGen
  
pairGenIntString :: Gen (Pair Int String)
pairGenIntString = pairGen

-- Greater than the sum of its parts
data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)
  -- equal odds for each

sumGenEqual :: (Arbitrary a,
  Arbitrary b) =>
  Gen (Sum a b)
sumGenEqual = do
  a <- Test.QuickCheck.arbitrary
  b <- Test.QuickCheck.arbitrary
  oneof [return $ First a,
    return $ Second b]

sumGenCharInt :: Gen (Sum Char Int)
sumGenCharInt = sumGenEqual

sumGenFirstPls :: (Arbitrary a,
  Arbitrary b) =>
  Gen (Sum a b)
sumGenFirstPls = do
  a <- Test.QuickCheck.arbitrary
  b <- Test.QuickCheck.arbitrary
  frequency [(10, return $ First a),
    (1, return $ Second b)]

sumGenCharIntFirst :: Gen (Sum Char Int)
sumGenCharIntFirst = sumGenFirstPls

-- CoArbitrary
-- TODO: move to separate module
data Bool' =
  True'
  | False'
  deriving (Generic)

instance CoArbitrary Bool'

trueGen :: Gen Int
trueGen = coarbitrary True' Test.QuickCheck.arbitrary

falseGen :: Gen Int
falseGen = coarbitrary False' Test.QuickCheck.arbitrary
