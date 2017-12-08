module Chapter4 where
-- find mistakes
  -- 1. not True && true -- won't compile (because of 'true')
  -- 2. not (x = 6) -- won't compile (x is not in scope, =)
  -- 3. (1 * 2) > 5 -- False
  -- 4. [Merry] > [Happy] -- won't compile (not strings, not in scope)
  -- 5. 9 -- won't compile (list of different types)

awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

-- ex 2:
-- a) length [1, 2, 3, 4, 5] - 5
-- b) length [(1, 2), (2, 3), (3, 4)] - 3
-- c) length allAwesome - 2
-- d) length (concat allAwesome) - 5

-- ex3:
-- 6 / length [1, 2, 3]

-- ex4:
-- 6 `div` length [1, 2, 3]

--  ex5 - Bool (True); ex6 - Bool (False)

-- ex7:
-- length allAwesome == 2 --works
-- length [1, 'a', 3, 'b'] -- doesn't
-- length allAwesome + length awesome --5
-- (8 == 8) && ('b' < 'a') --False
-- (8 == 8) && 9 -- won't compile

-- ex8:
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = x == reverse x

--9:
myAbs :: Integer -> Integer
myAbs x = if x < 0 then (-x) else x

--10:
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f x y =((snd x, snd y), (fst x, fst y))

-- Correcting syntax
--1)
x = (+)
func xs = w `x` 1
  where w = length xs

--2)
myId x = x

--3)
f1 = fst


-- Match the function names to their types
-- 1. Which of the following types is the type of show?
-- a) show a => a -> String
-- b) Show a -> a -> String
-- c) Show a => a -> String ++++
-- 2. Which of the following types is the type of (==)?
-- a) a -> a -> Bool
-- b) Eq a => a -> a -> Bool +++
-- c) Eq a -> a -> a -> Bool
-- d) Eq a => A -> Bool
-- 3. Which of the following types is the type of fst?
-- a) (a, b) -> a +++
-- b) b -> a
-- c) (a, b) -> b
-- 4. Which of the following types is the type of (+)?
-- a) (+) :: Num a -> a -> a -> Bool
-- b) (+) :: Num a => a -> a -> Bool
-- c) (+) :: num a => a -> a -> a
-- d) (+) :: Num a => a -> a -> a +++
-- e) (+) :: a -> a -> a
