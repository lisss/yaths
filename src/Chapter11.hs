{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE GADTs #-}

module Chapter11 where

import Data.Int(Int8)
import Data.Char
import Data.List
import Chapter9(myWords)
import Data.Ord (comparing)

-- Exercises: Dog Types
data DogueDeBordeaux doge = DogueDeBordeaux doge

data Doggies a =
  Husky a
  | Mastiff a
  deriving (Eq, Show)

-- 1. Is Doggies a type constructor or a data constructor? --> Type constructor
-- 2. What is the kind of Doggies? --> * -> *
-- 3. What is the kind of Doggies String? --> Doggies String :: *
-- 4. What is the type of Husky 10? --> Num a => Doggies a
-- 5. What is the type of Husky (10 :: Integer)? --> Doggies Integer
-- 6. What is the type of Mastiff "Scooby Doo"? --> Doggies String
-- 7. Is DogueDeBordeaux a type constructor or a data constructor? --> both
-- 8. What is the type of DogueDeBordeaux? --> a -> DogueDeBordeaux a
-- 9. What is the type of DogueDeBordeaux "doggie!" --> DogueDeBordeaux String

-- Exercises: Vehicles
data Price = Price Integer deriving (Eq, Show)

data Manufacturer =
  Mini
  | Mazda
  | Tata
  deriving (Eq, Show)
  
data Vehicle = Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

data Airline =
  PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

myCar = Car Mini (Price 14000)
urCar = Car Mazda (Price 20000)
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir

-- 1. :: Vehicle
-- 2.
isCar :: Vehicle -> Bool
isCar x
  | (Car _ _) <- x = True
  | otherwise = False

isPlane :: Vehicle -> Bool
isPlane x
  | (Plane _) <- x = True
  | otherwise = False

areCars :: [Vehicle] -> [Bool]
areCars x = map (\a -> isCar a) x

-- 3.
getManu :: Vehicle -> Manufacturer
getManu x
  | (Car x' _) <- x = x'
  -- will do better when using records
  | otherwise = undefined

-- 4. undefined so far:(
-- 5.
data Vehicle' = Car' Manufacturer Price
  | Plane' Airline Int
  deriving (Eq, Show)

isPlane' :: Vehicle' -> Bool
isPlane' x
  | (Plane' _ _) <- x = True
  | otherwise = False

-- Exercises: Cardinality
-- 1. data PugType = PugData --> 1
-- 2.
data Airline' =
  PapuAir'
  | CatapultsR'Us'
  | TakeYourChancesUnited'
-- --> 3
-- 3. 65445
-- 4. nothing
-- 5. ebu. 32 bit?


-- Exercises: For Example
data Example = MakeExample deriving Show
-- 1. a) :: Example; b) error
-- 2. Yup
-- 3.
data Example' = MakeExample' Int deriving Show
-- :: Int -> Example'

-- Exercises: Logic Goats
class TooMany a where
  tooMany :: a -> Bool

instance TooMany Int where
  tooMany n = n > 42

-- newtype Goats = Goats Int deriving Show

newtype Goats = Goats Int deriving (Eq, Show, TooMany)

-- 1.
-- newtype Type' = Type' (Int, String) deriving (Eq, Show, TooMany)
newtype IntString = IntString (Int, String)
instance TooMany IntString where
  tooMany (IntString x) = fst x > 42

-- 2.
newtype IntInt = IntInt (Int, Int)
instance TooMany IntInt where
  tooMany (IntInt x) = fst x + snd x > 42
  -- tooMany (IntInt x) = uncurry (+) x > 42 --> sexy one

-- 3.
-- data NumTooMany a where NumTooMany :: (Num a, TooMany a) => a -> NumTooMany (a, a) ?????
-- data NumTooMany a = NumTooMany (a, a)
-- instance (Num a, TooMany a) => TooMany (NumTooMany a) where
--   tooMany (NumTooMany x) = fst x == 42

-- Exercises: Pity the Bool
-- 1.
data BigSmall =
  Big Bool
  | Small Bool
  deriving (Eq, Show)
-- 4

-- 2.
data NumberOrBool =
  Numba Int8
  | BoolyBool Bool
  deriving (Eq, Show)

myNumba = Numba (-128)
-- a) 258; b) warn (error ??); c) warn (error ??); d) but not in GHC 8

-- Exercises: How Does Your Garden Grow?
-- 1.
data FlowerType = Gardenia
  | Daisy
  | Rose
  | Lilac
  deriving Show
  
type Gardener = String

data Garden =
  Garden Gardener FlowerType
  deriving Show

-- ????
data Garden' =
  Garden1' Gardener FlowerType
  | Garden2' Gardener FlowerType
  | Garden3' Gardener FlowerType
  | Garden4' Gardener FlowerType
  deriving Show

-- Exercises: The Quad
-- 1.
data Quad =
  One
  | Two
  | Three
  | Four
  deriving (Eq, Show)

-- how many different forms can this take?
-- eQuad :: Either Quad Quad --> 8

-- 2. prodQuad :: (Quad, Quad) --> 16
-- 3. funcQuad :: Quad -> Quad --> 256
-- 4. prodTBool :: (Bool, Bool, Bool) --> 6
-- 5. gTwo :: Bool -> Bool -> Bool --> 16
-- 6. fTwo :: Bool -> Quad -> Quad --> 4^(4*2) = 65536

-- Write map for BinaryTree
data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Show)

mapTree :: (a -> b)
  -> BinaryTree a
  -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) = Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
  Node (Node Leaf 3 Leaf)
  1
  (Node Leaf 4 Leaf)

mapExpected :: BinaryTree Integer
mapExpected =
  Node (Node Leaf 4 Leaf)
  2
  (Node Leaf 5 Leaf)

mapOkay :: IO ()
mapOkay =
  if mapTree (+1) testTree' == mapExpected
    then print "yup okay!"
    else error "test failed!"

-- Convert binary trees to lists

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = a : preorder left ++ preorder right

inorder :: BinaryTree a -> [a]
inorder Leaf = []
inorder (Node left a right) = preorder left ++ a : preorder right

postorder :: BinaryTree a -> [a]
postorder Leaf = []
postorder (Node left a right) = preorder left ++ preorder right ++ [a]

testTree :: BinaryTree Integer
testTree =
  Node (Node Leaf 1 Leaf)
  2
  (Node Leaf 3 Leaf)

testPreorder :: IO ()
testPreorder =
  if preorder testTree == [2, 1, 3]
  then putStrLn "Preorder fine!"
  else putStrLn "Bad news bears."

testInorder :: IO ()
testInorder =
  if inorder testTree == [1, 2, 3]
  then putStrLn "Inorder fine!"
  else putStrLn "Bad news bears."

testPostorder :: IO ()
testPostorder =
  if postorder testTree == [1, 3, 2]
  then putStrLn "Postorder fine!"
  else putStrLn "postorder failed check"

main :: IO ()
main = do
  testPreorder
  testInorder
  testPostorder

-- Write foldr for BinaryTree
foldTree :: (a -> b -> b) -> b -> BinaryTree a -> b
foldTree _ x Leaf = x
foldTree f x (Node left a right) = f a (foldTree f (foldTree f x right) left)

-- Chapter Exercises
-- Multiple choice
-- 1. a)
-- 2. c)
-- 3. b)
-- 4. c)

-- Ciphers --> see Cipher module

-- As-patterns
isSubseqOf :: (Eq a) => [a] -> [a] -> Bool
isSubseqOf [] [] = False
isSubseqOf [] (_ : _) = True
isSubseqOf (_:_) [] = False
isSubseqOf (x:xs) ys@(_:ys') = x `elem` ys && isSubseqOf xs ys'

-- 2.
capitalizeWords :: String -> [(String, String)]
capitalizeWords [] = []
capitalizeWords x = go (myWords x) where
  go xs' = map (\ x' -> (x', capitalizeWord x')) xs'

-- Language exercises
-- 1.
capitalizeWord :: String -> String
capitalizeWord [] = []
capitalizeWord (x:xs)= toUpper x : xs

-- 2.
capitalizeParagraph :: String -> String
capitalizeParagraph [] = []
capitalizeParagraph x = unwords . map capitalizeWord . words $ x

-- Phone exercise --> see Phone module

-- Hutton’s Razor
data Expr = Lit Integer | Add Expr Expr

-- 1.
eval :: Expr -> Integer
eval (Lit x) = x
eval (Add (Lit x) (Lit y)) = x + y

-- 2.
printExpr :: Expr -> String
printExpr (Lit x) = show x
printExpr (Add (Lit x) (Lit y)) = show x ++ " + " ++ show y
printExpr (Add x (Lit y)) = printExpr x ++ " + " ++ show y
printExpr (Add (Lit x) y) = show x ++ " + " ++ printExpr y

palindrome line1 =
  case line1 == reverse (map toLower line1) of
    True -> putStrLn "It's a palindrome!"
    False -> putStrLn "Nope!"
