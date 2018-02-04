module MainRS where

import Reader
import State
import Data.Maybe
import qualified Data.DList as DL

main :: IO ()
main = do
  print $ runReader getDogRM' pers
  print ">>> Reader: Chapter Exercises"
  print $ sequenceA [Just 3, Just 2, Just 1]
  print $ sequenceA [x, y]
  print $ sequenceA [xs, ys]
  print $ summed <$> ((,) <$> xs <*> ys)
  print $ fmap summed ((,) <$> xs <*> zs)
  print $ bolt 7
  print $ fmap bolt z
  print $ sequenceA [(>3), (<8), even] 7
  ------
  print $ foldr (&&) True $ sequA 30
  print $ sequA $ fromMaybe 0 s'
  print $ bolt $ fromMaybe 0 ys
  print ">>> State: Fizzbuzz"
  print ">>> fizzbuzzList"
  mapM_ putStrLn $ fizzbuzzList [1..100]
  print ">>> fizzbuzzFromTo"
  mapM_ putStrLn $ fizzbuzzFromTo 1 100
  print ">>> fizzbuzzList == fizzbuzzFromTo"
  print $ DL.toList (fizzbuzzList [1..10]) == fizzbuzzFromTo 1 10
