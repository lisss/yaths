module Chapter7 where

myNum :: Integer
myNum = 1

-- Exercises: Grab Bag
-- 1. Which (two or more) of the following are equivalent?
mTh1 x y z = x * y * z -- +++
mTh2 x y = \z -> x * y * z -- +++
mTh3 x = \y -> \z -> x * y * z -- +++
mTh4 = \x -> \y -> \z -> x * y * z -- +++

-- 2. The type of mTh (above) is Num a => a -> a -> a -> a.
-- Which is the type of mTh 3?
-- a) Integer -> Integer -> Integer
-- b) Num a => a -> a -> a -> a
-- c) Num a => a -> a
-- d) Num a => a -> a -> a +++

-- 3.
-- a) Rewrite the f function in the where clause.
addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = \n -> n + 1
-- b) Rewrite the following to use anonymous lambda syntax:
addFive = \x -> \y -> (if x > y then y else x) + 5
-- c) Rewrite the following so that it doesn’t use anonymous lambda syntax:
mflip = \f -> \x -> \y -> f y x

-- Pattern matching (examples)
newtype Username = Username String
newtype AccountNumber = AccountNumber Integer
data User = UnregisteredUser | RegisteredUser Username AccountNumber

printUser :: User -> IO ()
printUser UnregisteredUser =
  putStrLn "UnregisteredUser"
printUser (RegisteredUser
  (Username name)
  (AccountNumber acctNum)) =
  putStrLn $ name ++ " " ++ show acctNum

data WherePenguinsLive =
  Galapagos
  | Antarctica
  | Australia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

data Penguin = Peng WherePenguinsLive
  deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica Galapagos = False
isSouthAfrica Antarctica = False
isSouthAfrica Australia = False
isSouthAfrica SouthAmerica = False

isSouthAfrica' :: WherePenguinsLive -> Bool
isSouthAfrica' SouthAfrica = True
isSouthAfrica' _ = False

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

humboldt = Peng SouthAmerica
gentoo = Peng Antarctica
macaroni = Peng Antarctica
little = Peng Australia
galapagos = Peng Galapagos

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False
antarcticPenguin :: Penguin -> Bool
antarcticPenguin (Peng Antarctica) = True
antarcticPenguin _ = False

antarcticOrGalapagos :: Penguin -> Bool
antarcticOrGalapagos p =
  (galapagosPenguin p)
  || (antarcticPenguin p)


-- Exercises: Variety Pack
-- 1.
k (x, y) = x -- :: (a, b) -> a
k1 = k ((4-1), 10)
k2 = k ("three", (1 + 2)) -- :: [Char]; not the same as type of k1 and k3
k3 = k (3, True) -- c)

-- 2.
f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (x, _, z) (x', _, z') = ((x, x'), (z, z')) 

-- Exercises: Case Practice
-- 1.
functionC :: Ord a => a -> a -> a
functionC x y = case x > y of
  True -> x
  False -> y

-- 2.
ifEvenAdd2 :: Integral a => a -> a
ifEvenAdd2 n = case even n of
  True -> n + 2
  False -> n

-- 3.
nums :: (Ord a, Num a) => a -> a
nums x = case compare x 0 of
  LT -> -1
  GT -> 1
  _ -> 0

-- Exercises: Artful Dodgy
dodgy :: Num a => a -> a -> a
dodgy x y = x + y * 10

oneIsOne :: Num a => a -> a
oneIsOne = dodgy 1

oneIsTwo :: Num a => a -> a
oneIsTwo = flip dodgy 2

-- 2. dodgy 1 1 = 11
-- 3. dodgy 2 2 = 22
-- 4. dodgy 1 2 = 21
-- 5. dodgy 2 1 = 12
-- 6. oneIsOne 1 = 11
-- 7. oneIsOne 2 = 21
-- 8. oneIsTwo 1 = 21
-- 9. oneIsTwo 2 = 22
-- 10. oneIsOne 3 = 31
-- 11. oneIsTwo 3 = 23

-- Exercises: Guard Duty
avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.8 = 'B'
  | y >= 0.7 = 'C'
  | y >= 0.59 = 'D'
  | y < 0.59 = 'F'
  where y = x / 100

-- 1. 'F'
-- 2. Nope, it'll return 'C'
-- 3. b)
-- 4. Any list that has Eq instance
-- 5. Eq a => [a] -> Bool
-- 6. c)
-- 7. (Num a, Ord a) => a
-- 8. (Num a, Ord a, Num b) => a -> b

-- Composition/point free style
add :: Int -> Int -> Int
add x y = x + y

addPF :: Int -> Int -> Int
addPF = (+)

addOne :: Int -> Int
addOne = \x -> x + 1

addOnePF :: Int -> Int
addOnePF = (+1)

-- main :: IO ()
-- main = do
--   print (0 :: Int)
--   print (add 1 0) -- 1
--   print (addOne 0) -- 1
--   print (addOnePF 0) -- 1
--   print ((addOne . addOne) 0) -- 2
--   print ((addOnePF . addOne) 0) -- 2
--   print ((addOne . addOnePF) 0) -- 2
--   print ((addOnePF . addOnePF) 0) -- 2
--   print (negate (addOne 0)) -- -1
--   print ((negate . addOne) 0) -- -1
--   print ((addOne . addOne . addOne
--     . negate . addOne) 0) -- 2

-- Chapter Exercises

-- ##### Multiple choice
-- 1. d)
-- 2. f :: Char -> String, g :: String -> [String]; b)
-- 3. d)
-- 4. b)
-- 5. a)

-- ##### Let’s write code
-- 1.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

nsDigit :: Integral a => a -> a -> a
nsDigit n = flip mod 10 . fst . flip divMod n

tensDigit' :: Integral a => a -> a
tensDigit' = nsDigit 10

hunsD :: Integral a => a -> a
hunsD = nsDigit 100

-- 2.
foldBool' :: a -> a -> Bool -> a
foldBool' x y b = case (x, y, b) of
  (x, _, False) -> x
  (_, y, True) -> y

-- pattern guards (https://wiki.haskell.org/Pattern_guard)
foldBool'' :: a -> a -> Bool -> a
foldBool'' x y b
  | (x, _, False) <- (x, y, b) = x
  | (_, y, True) <- (x, y, b) = y

-- 3.
g :: (a -> b) -> (a, c) -> (b, c)
g f t = (f (fst t), snd t)

-- 4.
roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

-- main = do
--   print (roundTrip 4)
--   print (id 4)

-- 5.
roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show

-- main = do
--   print (roundTrip' 4)
--   print (id 4)

-- 6.
roundTrip'' :: (Show a, Read b) => a -> b
roundTrip'' = read . show

main = do
  print (roundTrip'' 4 :: Int)
  print (id 4)
