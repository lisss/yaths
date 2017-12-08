module AdditionTest where

import Test.Hspec
import Debug.Trace
import Test.QuickCheck

dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise =
            go (n - d) d (count + 1)

main :: IO ()
main = hspec $
  describe "Addition" $ do
    it "15 divided by 3 is 5" $
      dividedBy 15 3 `shouldBe` (5, 0)
    it "22 divided by 5 is\
      \ 4 remainder 2" $
      dividedBy 22 5 `shouldBe` (4, 2)


-- Intermission: Short Exercise
-- TODO: rewrite this piece of shit
myMult :: (Eq a, Num a, Ord a, Show a) => a -> a -> a
myMult x y = go x y where
  go x' y'
    | x' == 0 || y' == 0 = 0
    | x' == 1 = y'
    | y' == 1 = x'
    | x' == -1 = -y'
    | y' == -1 = -x'
    | x < 0 && y < 0 = go (abs x' + abs x) (abs y' - 1)
    | x > 0 && y < 0 = go (x' + x) (y' + 1)
    | otherwise = go (x' + x) (y' - 1)


multSpec :: IO ()
multSpec = hspec $
  describe "Multiplication" $ do
    it "Mult x by zero is 0" $
      myMult 15 0 `shouldBe` 0
    it "Mult -x by zero is 0" $
      myMult (-15) 0 `shouldBe` 0
    it "Mult x by 1 is x" $
      myMult 9 1 `shouldBe` 9
    it "Mult 3 by 8 is 24" $
      myMult 3 8 `shouldBe` 24
    it "Mult 3 by (-8) is -24" $
      myMult 3 (-8) `shouldBe` (-24)
    it "Mult (-3) by 8 is -24" $
      myMult (-3) 8 `shouldBe` (-24)
    it "Mult (-3) by (-8) is -24" $
      myMult (-3) (-8) `shouldBe` 24

multQCheck :: IO ()
multQCheck = hspec $
  describe "Multiplication" $
    it "x + 1 is always\
    \ greater than x" $
      property $ \x -> x + 1 > (x :: Int)

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

oneThroughThree' :: Gen Int
oneThroughThree' = elements [1, 2, 2, 2, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b)
  => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)
  
genThreeple :: (Arbitrary a, Arbitrary b,
  Arbitrary c)
  => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b)
  => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck does so
-- you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
    , (3, return (Just a))]

-- Using QuickCheck without Hspec
prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 1 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
