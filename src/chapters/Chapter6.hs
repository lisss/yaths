module Chapter6 where

import Data.List (sort)

data DayOfWeek =
  Mon | Tue | Weds | Thu | Fri | Sat | Sun

instance Eq DayOfWeek where
  (==) Mon Mon = True
  (==) Tue Tue = True
  (==) Weds Weds = True
  (==) Thu Thu = True
  (==) Fri Fri = True
  (==) Sat Sat = True
  (==) Sun Sun = True
  (==) _ _     = False

data Date = Date DayOfWeek Int

instance Eq Date where
  (==) (Date weekday dayOfMonth)
      (Date weekday' dayOfMonth') =
    weekday == weekday'
    && dayOfMonth == dayOfMonth'


-- Exercises: Eq Instances
-- 1.
data TisAnInteger = TisAn Integer
instance Eq TisAnInteger where
  TisAn a == TisAn a' = a == a'
-- 2.
data TwoIntegers = Two Integer Integer
instance Eq TwoIntegers where
  Two a b == Two a' b' = a == a' && b == b' 
-- 3.
data StringOrInt = TisAnInt Int | TisAString String
instance Eq StringOrInt where
  TisAnInt a == TisAnInt a' = a == a'
  TisAString a == TisAString a' = a == a'
  _ == _ = False

-- 4.
data Pair a = Pair a a
instance Eq a => Eq (Pair a) where
  (==) (Pair a b) (Pair a' b') = a == a' && b == b'

-- 5.
data Tuple a b = Tuple a b
instance (Eq a, Eq b) => Eq (Tuple a b) where
  Tuple a b == Tuple a' b' = a == a' && b == b'

-- 6.
data Which a = ThisOne a | ThatOne a
instance Eq a => Eq (Which a) where
  (==) (ThisOne a) (ThisOne a') = a == a'
  (==) (ThatOne a) (ThatOne a') = a == a'
  (==) _ _ = False

-- 7.
data EitherOr a b = Hello a | Goodbye b
instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (==) (Hello a) (Hello a') = a == a'
  (==) (Goodbye b) (Goodbye b') = b == b'
  (==) _ _ = False

-- Exercises: Will They Work?
-- 1) +, 2) +, 3) -, 4) +

-- data Mood a = Blah a deriving Show

-- Multiple choice
-- 1. c) 2. a)? b). 3. a) 4. c) 5. a)

-- Does it typecheck?
-- 1.
data Person = Person Bool deriving Show
printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)

-- 2.
data Mood = Blah
  | Woot deriving (Eq, Show)
settleDown x = if x == Woot
  then Blah
  else x

-- 4.
type Subject = String
type Verb = String
type Object = String
data Sentence =
  Sentence Subject Verb Object deriving (Eq, Show)
s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"


-- Given a datatype declaration, what can we do?
data Rocks =
  Rocks String deriving (Eq, Show, Ord)
data Yeah =
  Yeah Bool deriving (Eq, Show, Ord)
data Papu =
  Papu Rocks Yeah
  deriving (Eq, Show, Ord)

  -- 1.
phew = Papu (Rocks "chases") (Yeah True)
-- 2. ok
-- 3.
equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- 4.
comparePapus :: Papu -> Papu -> Bool
comparePapus p p' = p > p'

-- Match the types
-- 1.
i :: Int
i = 2

--2 - can't replace Float with Num
f :: Float
f = 1.0

--3
f2 :: Fractional a => a
f2 = 1.0

--4
f4 :: RealFrac a => a
f4 = 1.0

--5
freud :: Ord a => a -> a
freud x = x

--6
freud' :: Int -> Int
freud' x = x

--7 - myX is not a, so can't be used in a -> a
myX = 1 :: Int

sigmund :: Int -> Int
sigmund x = myX

--8 - doesn't work, as myX can't be used instead of Num
sigmund' :: Int -> Int
sigmund' x = myX

--9
jung :: [Int] -> Int
jung xs = head (sort xs)

--10
young :: Ord a => [a] -> a
young xs = head (sort xs)

--11 --mySort can sort only chars, to Ord doesn't work
mySort :: [Char] -> [Char]
mySort = sort

signifier :: [Char] -> Char
signifier xs = head (mySort xs)

--Type-Kwon-Do Two: Electric Typealoo
--1
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

--2
arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromIntegral i
