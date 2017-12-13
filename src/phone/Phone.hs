module Phone where

import Data.Char
import Data.List
import Data.Ord (comparing)

type Digit = Char
type Letters = String
type Presses = Int
newtype DaPhone = DaPhone [(Digit, Letters)] deriving Show

phone = DaPhone [
  ('1', ""),  
  ('2', "abc"),
  ('3', "def"),
  ('4', "ghi"),
  ('5', "jkl"),
  ('6', "mno"),
  ('7', "pqrs"),
  ('8', "tuv"),
  ('9', "wxyz"),
  ('0', "+_"),
  ('*', "^"),
  ('#', ".,")
  ]

convo :: [String]
convo =
  ["Wanna play 20 questions",
  "Ya",
  "U 1st haha",
  "Lol ok. Have u ever tasted alcohol",
  "Lol ya",
  "Wow ur cool haha. Ur turn",
  "Ok. Do u think I am pretty Lol",
  "Lol ya",
  "Just making sure rofl ur turn"]

getIndex :: (Digit, Letters) -> Char -> Maybe Presses
getIndex (d, letters) c
  | c == d && not (isDigit c) = Just 0
  | c == d && isDigit c = Just (length letters)
  | otherwise = elemIndex c letters

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)]
reverseTaps (DaPhone p) c
  | isSpace c = [('0', 1)]
  | isUpper c = ('*', 1) : go p (toLower c)
  | otherwise = go p c
  where go [] _ = []
        go (x : xs) c' = case getIndex x c' of
          Just i -> [(fst x, i + 1)]
          Nothing -> go xs c'

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)]
cellPhonesDead p = concatMap (\c -> reverseTaps p c)

fingerTaps :: [(Digit, Presses)] -> Presses
fingerTaps = foldr (\(_, p) acc -> p + acc) 0

typedLetters :: String -> [(Digit, Presses)]
typedLetters = map (\x -> (x, fingerTaps . reverseTaps phone $ x))

groupRes :: Ord a => [(a, Presses)] -> [(a, [Presses])]
groupRes = foldr f [] . sortBy (comparing fst)
  where 
    f (d, p) [] = [(d, [p])] 
    f (d, p) res@((x, y):xs)
      | x == d = (x, p:y):xs
      | otherwise = (d, [p]):res

groupSum :: String -> [(Digit, Presses)]
groupSum x = concatSum . groupRes . typedLetters $ x

concatSum :: [(a, [Presses])] -> [(a, Presses)]
concatSum = map (\ (a, b) -> (a, sum b))

getMax :: [(a, Presses)] -> (a, Presses)
getMax (x:xs) = go x xs
  where go acc [] = acc
        go (m, n) (p:ps)
          | n < snd p = go p ps
          | otherwise = go (m, n) ps

mostTypedLetter :: String -> (Digit, Presses)
mostTypedLetter = getMax . groupSum

-- | Test mostPopularLetter
-- >>> mostPopularLetter "adddvbyty"
-- 'y'
mostPopularLetter :: String -> Digit
mostPopularLetter = fst . mostTypedLetter

coolestLtr :: [String] -> Char
coolestLtr = fst . getMax . concatSum . groupRes . map mostTypedLetter

-- Not clear exercise requirement...
-- This one returns the most typed word itself
-- coolestWord :: [String] -> String
-- coolestWord x = fst . getMax . map (\x' -> (,) x' . fingerTaps . groupSum $ x') . concatMap words $ x

-- | Test coolestWord
-- >>> coolestWord convo
-- "Lol"
coolestWord :: [String] -> String
coolestWord [] = ""
coolestWord x = fst . getMax . concatSum . groupRes . map (\x' -> (,) x' . fingerTaps . groupSum $ x') . concatMap words $ x
