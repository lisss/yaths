-- Using QuickCheck
module QuickCheckTest where

import Test.QuickCheck
import Test.QuickCheck.Function
import Data.List (sort)
import Data.Char(toUpper)
import Chapter11(capitalizeWord)

-- 1.
half :: Double -> Double
half x = x / 2

halfIdentity :: Double -> Double
halfIdentity = (*2) . half

prop_halfIdentity :: Property
prop_halfIdentity = forAll arbitrary (\x -> x == halfIdentity x)

-- 2.
listOrdered :: (Ord a) => [a] -> Bool
listOrdered xs =
  snd $ foldr go (Nothing, True) xs
    where go _ status@(_, False) = status
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)

listGen :: Gen [Integer]
listGen = listOf arbitrary

prop_listOrdered :: Property
prop_listOrdered = forAll listGen (\x -> listOrdered (sort x))

-- 3.
plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

intGen :: Gen (Integer, Integer, Integer)
intGen = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

prop_plus = forAll intGen (\(x, y, z) -> plusAssociative x y z && plusCommutative x y)
-- 4.
multAssociative x y z = x * (y * z) == (x * y) * z
multCommutative x y = x * y == y * x

intGen1 :: Gen (Integer, Integer, Integer)
intGen1 = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

prop_mult = forAll intGen1 (\(x, y, z) -> multAssociative x y z && multCommutative x y)

-- 5.
quotRemRel x y = quot x y * y + rem x y == x
divModRel x y = div x y * y + mod x y == x

intGen2 :: Gen (Integer, Integer)
intGen2 = do
  a <- arbitrary
  b <- arbitrary `suchThat` (> 0)
  return (a, b)

prop_quotRemRel :: Property
prop_quotRemRel = forAll intGen2 (\(x, y) -> quotRemRel x y)
prop_divModRel = forAll intGen2 (\(x, y) -> divModRel x y)

-- 6. (^) is neither ass. nor comm.
expAssociative x y z = x ^ (y ^ z) == (x ^ y) ^ z
expCommutative x y = x ^ y == y ^ x

intGen3 :: Gen (Integer, Integer, Integer)
intGen3 = do
  a <- arbitrary
  b <- arbitrary `suchThat` (> 0)
  c <- arbitrary `suchThat` (> 0)
  return (a, b, c)

intGen4 :: Gen (Integer, Integer)
intGen4 = do
  a <- arbitrary
  b <- arbitrary `suchThat` (> 0)
  return (a, b)

prop_expAss = forAll intGen3 (\(x, y, z) -> expAssociative x y z)
prop_expComm = forAll intGen4 (\(x, y) -> expCommutative x y)

-- 7.
listGen1 :: Gen [Integer]
listGen1 = listOf arbitrary

prop_reverse :: Property
prop_reverse = forAll listGen1 $ \x -> (reverse . reverse $ x) == id x

-- 8.
-- f $ a = f a
-- f . g = \x -> f (g x)

prop_dollar :: Eq b => Fun a b -> a -> Bool
prop_dollar (Fn f) x = f x == (f $ x)

prop_dot :: Eq a => Fun b a -> Fun a b -> a -> Bool
prop_dot (Fn f) (Fn g) x = (f . g $ x) == (\x' -> f (g x')) x

-- 9. 
-- foldr (:) == (++)
-- foldr (++) [] == concat

listGen2 :: Gen [Integer]
listGen2 = listOf arbitrary

prop_foldr1 :: Property
prop_foldr1 = forAll listGen2 (\x -> foldr (:) [] x == (++) [] x)

listGen3 :: Gen [[Integer]]
listGen3 = listOf . listOf $ arbitrary

prop_foldr2 :: Property
prop_foldr2 = forAll listGen3 (\x -> foldr (++) [] x == concat x)

-- 10. Nope ('cause take could accept a value > list length)
f1 n xs = length (take n xs) == n

listGen4 :: Gen ([Int], Int)
listGen4 = do 
  list <- listOf arbitrary
  n <- arbitrary `suchThat` (\n -> n >= 0)
  return (list, n)

prop_length :: Property
prop_length = forAll listGen4 (\(xs, n) -> f1 n xs)

-- 11. 
f2 x = (read (show x)) == x

gen :: Gen Int
gen = arbitrary

prop_readShow :: Property
prop_readShow = forAll gen f2

-- Failure --> bacuse of floating point numbers
square x = x * x
squareIdentity = square . sqrt

-- Idempotence
twice f = f . f
fourTimes = twice . twice
-- 1.
f3 x = (capitalizeWord x == twice capitalizeWord x) && (capitalizeWord x == fourTimes capitalizeWord x)

stringGen :: Gen String
stringGen = arbitrary

prop_indempCapital :: Property
prop_indempCapital = forAll stringGen f3

-- 2. 
f' x = (sort x == twice sort x) && (sort x == fourTimes sort x)

listGen5 :: Gen [Integer]
listGen5 = listOf arbitrary

prop_indempSort :: Property
prop_indempSort = forAll listGen5 f'

-- Make a Gen random generator for the datatype
data Fool =
  Fulse
  | Frue
  deriving (Eq, Show)
  
-- 1. Equal probabilities for each.
-- instance Arbitrary Fool where
--   arbitrary = frequency [(1, return Fulse), (1, return Frue)]

-- 2. 2/3s chance of Fulse, 1/3 chance of Frue
instance Arbitrary Fool where
  arbitrary = frequency [(2, return Fulse), (1, return Frue)]

-- Hangman testing --> TODO: move everything to a single project

main :: IO ()
main = do
  quickCheck prop_halfIdentity
  quickCheck prop_listOrdered
  quickCheck prop_plus
  quickCheck prop_mult
  quickCheck prop_quotRemRel
  quickCheck prop_divModRel
  quickCheck prop_expAss
  quickCheck prop_expComm
  quickCheck prop_reverse
  quickCheck (prop_dollar :: Fun Int String -> Int -> Bool)
  quickCheck (prop_dot :: Fun Int Char -> Fun Char Int -> Char -> Bool)
  quickCheck prop_foldr1
  quickCheck prop_foldr2
  quickCheck prop_length
  quickCheck prop_readShow
  quickCheck prop_indempCapital
  quickCheck prop_indempSort
