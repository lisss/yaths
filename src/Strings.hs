module Strings where

import Data.Char(isSpace)

exclaim x = x ++ "!"

takeFive x = take 1 $ drop 4 x

takeLastCharOfHead x = take 1 $ reverse $ takeWhile (not . isSpace) x

thirdLetter = (!! 2)

letterIndex = (!!) "Curry is awesome!"

rvrs :: String -> String
rvrs x = a ++ b ++ c where
  c = take 5 x
  b = take 4 $ drop 5 x
  a = drop 9 x

main :: IO ()
main = print $ rvrs "Curry is awesome"
