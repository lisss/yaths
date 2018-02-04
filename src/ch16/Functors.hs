{-# LANGUAGE FlexibleInstances #-}
module Functors (
  Optional(Nada,Only)
  ,Identity(Identity)
  ,List(Nil,Cons)
  ,Pair(Pair)
  ,Two(Two)
  ,Three(Three)
  ,Three'(Three')
  ,Four(Four)
  ,Four'(Four')
  ,Four''(Four'')
  ,Sum(First, Second)
  ,main
  ) where

import Test.QuickCheck
import Test.QuickCheck.Function
import GHC.Arr
import Monoid_Semigroup (Optional(Nada,Only))

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

-- Exercise: Possibly
data Possibly a =
  LolNope
  | Yeppers a
  deriving (Eq, Show)

instance Functor Possibly where
  fmap _ LolNope = LolNope
  fmap f (Yeppers a) = Yeppers (f a)

-- Short Exercise
data Sum a b =
  First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
  fmap _ (First a) = First a
  fmap f (Second b) = Second (f b)

-- Chapter exercises
-- 1. NOPE
-- data Bool = False | True

-- 2.
data BoolAndSomethingElse a = False' a | True' a
instance Functor BoolAndSomethingElse where
  fmap f (False' a) = False' (f a)
  fmap f (True' a) = True' (f a)

-- 3.
data BoolAndMaybeSomethingElse a = Falsish | Truish a
instance Functor BoolAndMaybeSomethingElse where
  fmap _ Falsish = Falsish
  fmap f (Truish a) = Truish (f a)

-- 4. --> NOPE
newtype Mu f = InF { outF :: f (Mu f) }

-- 5. --> NOPE
data D = D (Array Word Word) Int Int

-- Rearrange the arguments 
-- 1.
data Sum' b a =
  First' a
  | Second' b

instance Functor (Sum' e) where
  fmap f (First' a) = First' (f a)
  fmap f (Second' b) = Second' b

-- 2.
data Company a b c =
  DeepBlue a b
  | Something c

instance Functor (Company e e') where
  fmap f (Something b) = Something (f b)
  fmap _ (DeepBlue a c) = DeepBlue a c

-- 3.
data More a b = L b a b | R a b a
  deriving (Eq, Show)

instance Functor (More x) where
  fmap f (L a b a') = L (f a) b (f a')
  fmap f (R b a b') = R b (f a) b'

-- Write Functor instances for the following datatypes.
-- 1.
data Quant a b =
  Finance
  | Desk a
  | Bloor b

instance Functor (Quant a) where
  fmap _ Finance = Finance
  fmap _ (Desk a) = Desk a
  fmap f (Bloor a) = Bloor (f a)

-- 2.
newtype K a b = K a

instance Functor (K a) where
  fmap f (K a) = K a

-- 3.
newtype Flip f a b =
  Flip (f b a)
  deriving (Eq, Show)

instance Functor (Flip K a) where
  fmap f (Flip (K a)) = Flip $ K (f a)

-- 4.
newtype EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
  fmap f (GoatyConst a) = GoatyConst (f a)

-- 5.
newtype LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
  fmap f (LiftItOut fa) = LiftItOut (fmap f fa)

-- 6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
  fmap f (DaWrappa fa ga) = DaWrappa (fmap f fa) (fmap f ga)

-- 7.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor f, Functor g) => Functor (IgnoreOne f g a) where
  fmap f (IgnoringSomething fa ga) = IgnoringSomething fa (fmap f ga)

-- 8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where
  fmap f (Notorious ga go gt) = Notorious ga go (fmap f gt)

-- 9.
data List a = Nil | Cons a (List a) deriving (Eq, Show)
instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons a l) = Cons (f a) (fmap f l)

-- 10.
data GoatLord a =
  NoGoat
  | OneGoat a
  | MoreGoats (GoatLord a)
  (GoatLord a)
  (GoatLord a)

-- TODO: write tests (in test dir) >>>>
instance Functor GoatLord where
  fmap _ NoGoat = NoGoat
  fmap f (OneGoat a) = OneGoat (f a)
  fmap f (MoreGoats a b c) = MoreGoats (fmap f a) (fmap f b) (fmap f c)

-- 11.
data TalkToMe a =
  Halt
  | Print String a
  | Read (String -> a)

instance Functor TalkToMe where
  fmap _ Halt = Halt
  fmap f (Print a b) = Print a (f b)
  fmap f (Read f') = Read (fmap f f')

-- Extra
instance Functor Optional where
  fmap _ Nada = Nada
  fmap f (Only a) = Only (f a)

data Four'' a b = Four'' a b b b deriving (Eq, Show)

instance Functor (Four'' a) where
  fmap f (Four'' a b b' b'') = Four'' a (f b) (f b') (f b'')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Four'' a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Four'' a b b b)

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
