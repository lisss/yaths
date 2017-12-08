{-# LANGUAGE NoMonomorphismRestriction #-}
module Chapter5 where

  -- 1. Functions:
  -- a) not - c)
  -- b) length - d)
  -- c) concat - b)
  -- d) head - a)
  -- e) (<) - e)
  -- 2. Type signatures:
  -- a) _ :: [a] -> a
  -- b) _ :: [[a]] -> [a]
  -- c) _ :: Bool -> Bool
  -- d) _ :: [a] -> Int
  -- e) _ :: Ord a => a -> a -> Bool

-- Currying and uncurrying:
nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

curriedFunction :: Integer  -> Bool  -> Integer
curriedFunction i b =  i + nonsense b

uncurriedFunction :: (Integer, Bool)  -> Integer
uncurriedFunction (i, b) =   i + nonsense b

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + nonsense b

anonNested :: Integer  -> Bool  -> Integer
anonNested =  \i -> \b -> i + nonsense b

curry :: ((t1, t2) -> t) -> t1 -> t2 -> t
curry f a b = f (a, b)
-- curry fst 'q' 3 = 'q'

-- Sectioning
x = 5
y = (2^)
z = (^2)
-- y x = 32
-- z x = 25

c_ = (`elem` [1..10])
-- also could be written as:
c1 x = elem x [1..10]
c2 = \x -> elem x [1..10]

-- Exercises: Type Arguments
f :: a -> a -> a -> a; f = undefined
x1 :: Char; x1 = undefined
-- a) f x1 :: Char -> Char -> Char

g :: a -> b -> c -> b; g = undefined
-- d) g 0 'c' "woot" :: Char

h :: (Num a, Num b) => a -> b -> b; h = undefined
-- d) h 1.0 2 :: Num b => b
-- c) h 1 (5.5 :: Double) :: Double

jackal :: (Ord a, Eq b) => a -> b -> a; jackal = undefined
-- a) jackal "keyboard" "has the word jackal in it" :: [Char]
-- e) jackal "keyboard" :: Eq b => b -> [Char]

kessel :: (Ord a, Num b) => a -> b -> a; kessel = undefined
-- b) kessel 1 2 :: (Num a, Ord a) => a
-- b) kessel 1 (2 :: Integer) :: (Num a, Ord a) => a
-- d) kessel (1 :: Integer) 2 :: Integer

-- Exercises: Parametricity
pfn1 :: a -> a -> a
pfn1 x y = x
-- 2nd option: pfn1 x y = y

pfn2 :: a -> b -> b
pfn2 x y = y

-- Type inference
tif1 :: Num a => a -> a -> a
tif1 x y = x + y + 3

tif2 x y = x + y + 3

-- Exercises: Apply Yourself

-- 1.
-- (++) :: [a] -> [a] -> [a]
myConcat x = x ++ " yo" -- :: [Char] -> [Char]

-- 2.
-- (*) :: Num a => a -> a -> a
-- Applied to a value
myMult x = (x / 3) * 5 -- :: Fractional a => a -> a // because of `/` function provided by Fractional typeclass

-- 3.
-- take :: Int -> [a] -> [a]
myTake x = take x "hey you" -- :: Int -> [Char]
-- 4.
-- (>) :: Ord a => a -> a -> Bool
myCom x = x > length [1..10] -- :: Int -> Bool
-- 5.
-- (<) :: Ord a => a -> a -> Bool
myAlph x = x < 'z' -- :: Char -> Bool

triple = tripleItYo
  where tripleItYo :: Integer -> Integer
        tripleItYo y = y * 3

-- Chapter Exercises

-- Multiple choice
-- 1. A value of type [a] is
  -- a) a list of alphabetic characters
  -- b) a list of lists
  -- c) a list whose elements are all of some type 𝑎 +++
  -- d) a list whose elements are all of different types
-- 2. A function of type [[a]] -> [a] could
  -- a) take a list of strings as an argument +++
  -- b) transform a character into a string
  -- c) transform a string into a list of strings
  -- d) take two arguments
-- 3. A function of type [a] -> Int -> a
  -- a) takes one argument +++
  -- b) returns one element of type 𝑎 from a list +++
  -- c) must return an Int value
  -- d) is completely fictional
-- 4. A function of type (a, b) -> a
  -- a) takes a list argument and returns a Char value
  -- b) has zero arguments
  -- c) takes a tuple argument and returns the first value +++
  -- d) requires that 𝑎 and 𝑏 be of different types


-- Determine the type
example = 1

-- 1. All function applications return a value. Determine the value
--  returned by these function applications and the type of that
--  value.
--   a) (* 9) 6 --> Num a => a; 54
--   b) head [(0,"doge"),(1,"kitteh")] --> Num a => (a, [Char]); (0,"doge")
--   c) head [(0 :: Integer ,"doge"),(1,"kitteh")] --> (Integer, [Char]); (0,"doge")
--   d) if False then True else False --> Bool; False
--   e) length [1, 2, 3, 4, 5] --> Int; 5
--   f) (length [1, 2, 3, 4]) > (length "TACOCAT") --> Bool; False

-- 2.
--   x = 5 --> Num a => a; 5
--   y = x + 5 --> Num a => a; 10
--   w = y * 10 --> Num a => a; 100
--   What is the type of w? --> Num a => a 
-- 3.
--   x = 5 --> Num a => a; 5
--   y = x + 5 --> Num a => a; 10
--   z y = y * 10 --> Num a => a -> a; 100
--   What is the type of z? --> Num a => a -> a

-- 4.
--   x = 5 --> Num a => a; 5
--   y = x + 5 --> Num a => a; 10
--   f = 4 / y --> Num a => a; 0.4
--   What is the type of f? --> Fractional a => a (!!! not Num !!!)

-- 5.
--   x = "Julie" --> [Char]; "Julie"
--   y = " <3 " --> [Char]; " <3 "
--   z = "Haskell" --> [Char]; "Haskell"
--   f = x ++ y ++ z --> [Char]; "Julie <3 Haskell"
--   What is the type of f? --> [Char]


-- Does it compile?
-- 1.
  -- bigNum = (^) 5 $ 10 +++
  -- wahoo = bigNum $ 10 ---; wahoo = bigNum
-- 2.
  -- x = print +++
  -- y = print "woohoo!" +++
  -- z = x "hello world" +++
-- 3.
  -- a = (+) +++
  -- b = 5 +++
  -- c = b 10 ---; c = b
  -- d = c 200 ---; d = c
-- 4.
  -- a = 12 + b +++
  -- b = 10000 * c +++

-- Type variable or specific type constructor?
-- * FP = fully polimorphic; CP = constrained polimorphic; CT = concrete type
-- f :: zed -> Zed -> Blah --> FP, CT, CT
-- f :: Enum b => a -> b -> C --> FP, CP, CT
-- f :: f -> g -> C --> FP, FP, CT

-- Write a type signature
functionH :: [a] -> a
functionH (x:_) = x
functionC :: Ord a => a -> a -> Bool
functionC x y = if (x > y) then True else False
functionS :: (a, b) -> b
functionS (x, y) = y

-- Given a type, write the function
myFunc :: (x -> y) -> (y -> z) -> c -> (a, x) -> (a, z)
myFunc xToY yToZ _ (a, x) = (a, yToZ (xToY x))

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c'' :: b -> a -> b -- is alpha equivalent to c
c'' x _ = x

c' :: a -> b -> b
c' _ y = y

r :: [a] -> [a]
r (_:xs) = xs

r' :: [a] -> [a]
r' = take 1

r'' :: [a] -> [a]
r'' = drop 1

co :: (b -> c) -> (a -> b) -> a -> c
co b2c a2b a = b2c $ a2b a

a :: (a -> c) -> a -> a
a _ a = a

a' :: (a -> b) -> a -> b
a' a2b = a2b

-- Fix it
fstString :: [Char] -> [Char]
fstString x = x ++ " in the rain"

sndString :: [Char] -> [Char]
sndString x = x ++ " over the rainbow"

sing = if (x > y) then fstString x else sndString y
  where x = "Singin"
        y = "Somewhere"
      
main :: IO ()
main = do
  print $ 1 + 2
  putStrLn "10"
  print $ negate (-1)
  print ((+) 0 blah)
    where blah = negate 1

-- Type-Kwon-Do

-- data Woot
-- data Blah

-- f_ :: Woot -> Blah
-- f_ = undefined

-- g_ :: (Blah, Woot) -> (Blah, Blah)
-- g_ (b, w) = (b, f_ w)

-- 1.
f_ :: Int -> String
f_ = undefined
g_ :: String -> Char
g_ = undefined

h_ :: Int -> Char
h_ = g_ . f_

-- 2.
data A
data B
data C

q_ :: A -> B
q_ = undefined
w_ :: B -> C
w_ = undefined

e_ :: A -> C
e_ = w_ . q_

-- 3.
data X
data Y
data Z

xz :: X -> Z
xz = undefined
yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform x = (xz $ fst x, yz $ snd x)

-- 4.
munge :: (x -> y)
      -> (y -> (w, z))
      -> x
      -> w
munge x2y y2wz x = fst $ y2wz $ x2y x
