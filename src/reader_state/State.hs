{-# LANGUAGE InstanceSigs #-}
module State where

import System.Random
import Control.Monad
import Control.Monad.Trans.State
import qualified Data.DList as DL

data Die =
  DieOne
  | DieTwo
  | DieThree
  | DieFour
  | DieFive
  | DieSix
  deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
  case n of
  1 -> DieOne
  2 -> DieTwo
  3 -> DieThree
  4 -> DieFour
  5 -> DieFive
  6 -> DieSix
  -- Use 'error'
  -- _extremely_ sparingly.
  x ->
    error $
    "intToDie got non 1-6 integer: "
    ++ show x

-- Exercises: Roll Your Own
rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
  where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= 20 = count
    | otherwise =
      let (die, nextGen) =
            randomR (1, 6) gen
      in go (sum + die)
      (count + 1) nextGen

-- 1.
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
  where
  go :: Int -> Int -> StdGen -> Int
  go sum count gen
    | sum >= n = count
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die)
      (count + 1) nextGen

-- 2.
rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 g []
  where
  go :: Int -> Int -> StdGen -> [Die] -> (Int, [Die])
  go sum count gen dies
    | sum >= n = (count, dies)
    | otherwise =
      let (die, nextGen) = randomR (1, 6) gen
      in go (sum + die) (count + 1) nextGen $ intToDie die : dies

-- Write State for yourself
newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
  fmap :: (a -> b) -> Moi s a -> Moi s b
  fmap f (Moi g) = Moi $ \x ->
    let (a, s) = g x
    in (f a, s)

instance Applicative (Moi s) where
  pure :: a -> Moi s a
  pure a = Moi $ \s -> (a, s)

  (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
  (<*>) (Moi f) (Moi g) = Moi $ \x ->
    let (fab, s) = f x
        (a, s') = g s
    in (fab a, s')

instance Monad (Moi s) where
  return = pure

  (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
  (>>=) (Moi f) g = Moi $ \x ->
    let (a, s) = f x
        (Moi sb) = g a
    in sb s

-- Fizzbuzz Differently
fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
  | n `mod` 5 == 0 = "Buzz"
  | n `mod` 3 == 0 = "Fizz"
  | otherwise = show n

addResult :: Integer -> State (DL.DList String) ()
addResult n = do
  xs <- get
  let result = fizzBuzz n
  -- snoc appends to the end, unlike
  -- cons which adds to the front
  put (DL.snoc xs result)

fizzbuzzList :: [Integer] -> DL.DList String
fizzbuzzList list = execState (mapM_ addResult list) DL.empty

-- Excersice
fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo f t
  | f == t = [fizzBuzz f]
  | f < t = fizzBuzz f : fizzbuzzFromTo (f + 1) t
  | otherwise = fizzbuzzFromTo t f

-- Chapter exercises
-- 1.
get' :: Moi s s
get' = Moi $ \s -> (s, s)

-- 2.
put' :: s -> Moi s ()
put' s = Moi $ const ((), s)

-- 3.
exec :: Moi s a -> s -> s
exec st = snd . runMoi st

-- 4.
eval :: Moi s a -> s -> a
eval st = fst . runMoi st

-- 5.
modify :: (s -> s) -> Moi s ()
modify f = Moi $ \s -> ((), f s)
