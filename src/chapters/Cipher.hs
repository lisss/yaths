module Cipher where

import Data.Char
import System.IO

caesar :: [Char] -> Int -> [Char]
caesar [] _ = []
caesar (x:xs) offset
  | diff < 0 = chr (ord x + offset) : caesar xs offset
  | otherwise = chr (ord 'a' + diff) : caesar xs offset
  where diff = mod (ord x) (ord 'a') - 26 + offset

unCaesar :: [Char] -> Int -> [Char]
unCaesar [] _ = []
unCaesar (x:xs) offset
  | diff >= 0 = chr (ord x - offset) : unCaesar xs offset
  | otherwise = chr (ord 'z' + diff + 1) : unCaesar xs offset
  where diff = mod (ord x) (ord 'a') - offset

-- Chapter 11
-- NOTE: this works only with lowercase letters

repeat' :: [Char] -> [Char] -> [Char]
repeat' [] _ = []
repeat' a [] = a
repeat' (x':xs) (y':ys)
  | x' == ' ' = x' : repeat' xs (y': ys)
  | otherwise = y' : repeat' xs (ys ++ [y'])
  
vigenere :: [Char] -> [Char]
vigenere [] = []
vigenere x = go x (repeat' x "ally") where
  go [] _ = []
  go _ [] = []
  go (x': xs) (y' : ys)
    | y' == ' ' = ' ' : go xs ys
    | otherwise = caesar [x'] (ord y' - ord 'a') ++ go xs ys

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  putStrLn "Enter ur fckn phrase please..."
  word <- getLine
  putStrLn (vigenere word)
