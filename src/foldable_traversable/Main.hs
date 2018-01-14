module Main where

import Foldable
import Traversable
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

identity_trigger = undefined :: Identity (Int, Int, [Int])
constant_trigger = undefined :: Constant (Int, Int, [Int]) (Int, Int, [Int])
optional_trigger = undefined :: Optional (Int, Int, [Int])
list_trigger = undefined :: List (Int, Int, [Int])
three_trigger = undefined :: Three (Int, Int, [Int]) (Int, Int, [Int]) (Int, Int, [Int])
pair_trigger = undefined :: Two (Int, Int, [Int]) (Int, Int, [Int])
big_trigger = undefined :: Three' (Int, Int, [Int]) (Int, Int, [Int])
bigger_trigger = undefined :: Four'' (Int, Int, [Int]) (Int, Int, [Int])

main = do
  quickBatch (traversable identity_trigger)
  quickBatch (traversable constant_trigger)
  quickBatch (traversable optional_trigger)
  quickBatch (traversable list_trigger)
  quickBatch (traversable three_trigger)
  quickBatch (traversable pair_trigger)
  quickBatch (traversable big_trigger)
  quickBatch (traversable bigger_trigger)