module Functors where

import Test.QuickCheck
import Test.QuickCheck.Function

replaceWithP :: b -> Char
replaceWithP = const 'p'

lms :: [Maybe [Char]]
lms = [Just "Ave", Nothing, Just "woohoo"]

-- Just making the argument more specific
replaceWithP' :: [Maybe [Char]] -> Char
replaceWithP' = replaceWithP

-- Prelude> :t fmap replaceWithP
-- fmap replaceWithP :: Functor f
-- => f a -> f Char
liftedReplace :: Functor f => f a -> f Char
liftedReplace = fmap replaceWithP

liftedReplace' :: [Maybe [Char]] -> [Char]
liftedReplace' = liftedReplace

twiceLifted :: (Functor f1, Functor f) =>
  f (f1 a) -> f (f1 Char)
twiceLifted = (fmap . fmap) replaceWithP

-- Making it more specific
twiceLifted' :: [Maybe [Char]]
  -> [Maybe Char]
twiceLifted' = twiceLifted

thriceLifted ::
  (Functor f2, Functor f1, Functor f)
  => f (f1 (f2 a)) -> f (f1 (f2 Char))
thriceLifted =
  (fmap . fmap . fmap) replaceWithP

-- More specific or "concrete"
thriceLifted' :: [Maybe [Char]]
  -> [Maybe [Char]]
thriceLifted' = thriceLifted

-- Exercises: Heavy Lifting
-- 1.
a = fmap (+1) $ read "[1]" :: [Int]
-- 2.
b = (fmap . fmap . fmap) (++ "lol") Just ["Hi,", "Hello"]
-- 3.
c = fmap (*2) (\x -> x - 2)
-- 4.
d =
  fmap ((return '1' ++) . show)
  (\x -> [x, 1..3])
-- 5.
-- e :: IO Integer
-- e = let ioi = readIO "1" :: IO Integer
--         changed = read ("123"++) $ fmap show ioi
--     in fmap (*3) changed

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (read . ("123" ++) . show) ioi
    in fmap (*3) changed

-- QuickChecking Functor instances
functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f)
  => (a -> b) -> (b -> c)
  -> f a
  -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

fc = functorCompose (+1) (*2)
li x = fc (x :: [Int])

functorCompose' :: (Eq (f c), Functor f) =>
  f a
  -> Fun a b
  -> Fun b c
  -> Bool
functorCompose' x (Fun _ f) (Fun _ g) =
  (fmap (g . f) x) == (fmap g . fmap f $ x)

type IntToInt = Fun Int Int

type IntFC = 
  [Int]
  -> IntToInt
  -> IntToInt
  -> Bool

-- Exercises: Instances of Func
-- 1.
newtype Identity a = Identity a deriving (Eq, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

-- 2.
data Pair a = Pair a a deriving (Eq, Show)

instance Functor Pair where
  fmap f (Pair a b) = Pair (f a) (f b)

instance Arbitrary a => Arbitrary (Pair a) where
  arbitrary = do
    a <- arbitrary
    return (Pair a a)

-- 3. 
data Two a b = Two a b deriving (Eq, Show)

instance Functor (Two a) where
  fmap f (Two a b) = Two a (f b)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

-- 4. 
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
  fmap f (Three a b c) = Three a b (f c)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

-- 5.
data Three' a b = Three' a b b deriving (Eq, Show)

instance Functor (Three' a) where
  fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Three' a b b)

-- 6. 
data Four a b c d = Four a b c d deriving (Eq, Show)

instance Functor (Four a b c) where
  fmap f (Four a b c d) = Four a b c (f d)

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => 
  Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

-- 7. 
data Four' a b = Four' a a a b deriving (Eq, Show)

instance Functor (Four' a) where
  fmap f (Four' a b c d) = Four' a b c (f d)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four' a a a b)

-- 8. Can you implement one for this type? Why? Why not?
-- --> NOPE, because its constructor is a constant
data Trivial = Trivial

--- MAIN ---
main :: IO ()
main = do
  putStr "replaceWithP' lms: "
  print (replaceWithP' lms)
  putStr "liftedReplace lms: "
  print (liftedReplace lms)
  putStr "liftedReplace' lms: "
  print (liftedReplace' lms)
  putStr "twiceLifted lms: "
  print (twiceLifted lms)
  putStr "twiceLifted' lms: "
  print (twiceLifted' lms)
  putStr "thriceLifted lms: "
  print (thriceLifted lms)
  putStr "thriceLifted' lms: "
  print (thriceLifted' lms)
  quickCheck (functorIdentity :: [Int] -> Bool)
  quickCheck li
  quickCheck (functorCompose' :: IntFC)
  -- --- >>> Exercises: Instances of Func <<<
  quickCheck (functorIdentity :: Identity Int -> Bool)
  quickCheck (functorCompose' :: Identity Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Pair Int -> Bool)
  quickCheck (functorCompose' :: Pair Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Two Int Int -> Bool)
  quickCheck (functorCompose' :: Two Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Three Int Int Int -> Bool)
  quickCheck (functorCompose' :: Three Int Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Three' Int Int -> Bool)
  quickCheck (functorCompose' :: Three' Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Four Int Int Int Int -> Bool)
  quickCheck (functorCompose' :: Four Int Int Int Int -> IntToInt -> IntToInt -> Bool)
  quickCheck (functorIdentity :: Four' Int Int -> Bool)
  quickCheck (functorCompose' :: Four' Int Int -> IntToInt -> IntToInt -> Bool)
