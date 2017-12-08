module Snippets where

f :: Eq a => [a] -> Bool
f = all =<< (==) . head

-- converting `=<<` to `>>=`

f1 :: Eq a => [a] -> Bool
f1 = ((==) . head) >>= all

-- By definition of `instance Monad ((->) r)`
-- f >>= k = \ r -> k (f r) r

f2 :: Eq a => [a] -> Bool
f2 = \r -> all (((==) . head) r) r

-- `a . b` == `\x -> a (b x)`

f3 :: Eq a => [a] -> Bool
f3 = \r -> all ((\x -> ((head x) ==)) r) r

-- moving `\r` into a function argument

f4 :: Eq a => [a] -> Bool
f4 r = all ((\x -> ((head x) ==)) r) r

-- apply lambda to `r`

f5 :: Eq a => [a] -> Bool
f5 r = all (head r ==) r
