module CipherTest where

import Test.QuickCheck
import Cipher(caesar, unCaesar, vigenere, unVigenere)

genCaesar :: Gen (String, Int)
genCaesar = do
  w <- listOf $ elements ['a'..'z']
  num <- arbitrary
  return (w, num)

genVigenere :: Gen String
genVigenere = listOf $ elements ['a'..'z']

prop_caesar :: Property
prop_caesar = forAll genCaesar (\(x, num) -> unCaesar (caesar x num) num == x)

prop_vigenere :: Property
prop_vigenere = forAll genVigenere (\x -> unVigenere (vigenere x) == x)

main :: IO ()
main = do
  quickCheck prop_caesar
  quickCheck prop_vigenere
