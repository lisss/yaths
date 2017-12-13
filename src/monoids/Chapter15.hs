-- {-# LANGUAGE FlexibleContexts #-}

module Chapter15 where

import Data.Monoid
import qualified Data.Semigroup as S
import Test.QuickCheck
import Test.QuickCheck.Function  

-- Exercise: Optional Monoid

data Optional a =
  Nada
  | Only a
  deriving (Eq, Show)
  
instance Monoid a => Monoid (Optional a) where
  mempty = Nada

  mappend x Nada = x
  mappend Nada x = x
  mappend (Only x) (Only y) = Only (mappend x y)

-- QuiuckCheck
monoidAssoc :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

-- Exercise: Maybe Another Monoid
newtype First' a =
  First' { getFirst' :: Optional a }
  deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency
        [(1, return First' { getFirst' = Only a })
        , (1, return First' { getFirst' = Nada })]

-- TODO: don't use Monoid type constraint (how???)
newtype Maybe' a = Maybe' { getMaybe :: Maybe a }

instance Monoid a => Monoid (Maybe' a) where
  mempty = Maybe' Nothing

  mappend (Maybe' Nothing) x = x
  mappend x (Maybe' Nothing) = x
  mappend (Maybe' x ) (Maybe' y) = Maybe' (x <> y)

-- TODO: First' (Only 1) `mappend` First' Nada --> doesn't work. Fix it
instance Monoid a => Monoid (First' a) where
  mempty = First' { getFirst' = Nada }

  mappend First' { getFirst' = Nada } x = x
  mappend x First' { getFirst' = Nada } = x
  mappend (First' x) (First' y) = First' (x <> y)

firstMappend :: Monoid a => First' a
  -> First' a
  -> First' a
firstMappend = mappend

type FirstMappend =
  First' String
  -> First' String
  -> First' String
  -> Bool
type FstId =
  First' String -> Bool

-- Chapter exercises
-- Semigroup exercises
-- 1.
data Trivial = Trivial deriving (Eq, Show)

instance S.Semigroup Trivial where
  _ <> _ = Trivial

instance Arbitrary Trivial where
  arbitrary = return Trivial

semigroupAssoc :: (Eq m, S.Semigroup m)
  => m -> m -> m -> Bool
semigroupAssoc a b c =
  (a S.<> (b S.<> c)) == ((a S.<> b) S.<> c)

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool

-- 2.
newtype Identity a = Identity a deriving (Eq, Show)

instance S.Semigroup a => S.Semigroup (Identity a) where
  Identity x <> Identity y = Identity (x S.<> y)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

type IdentAssoc a = Identity a -> Identity a -> Identity a -> Bool

-- 3. 
data Two a b = Two a b deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b) => S.Semigroup (Two a b) where
  Two x y <> Two x' y' = Two (x S.<> x') (y S.<> y')

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    return (Two a b)

type TwoAssoc a b = Two a b -> Two a b -> Two a b -> Bool

-- 4. 
data Three a b c = Three a b c deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b, S.Semigroup c)
  => S.Semigroup (Three a b c) where
  Three x y z <> Three x' y' z' = Three (x S.<> x') (y S.<> y') (z S.<> z')

instance (Arbitrary a, Arbitrary b, Arbitrary c)
  => Arbitrary (Three a b c) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return (Three a b c)

type ThreeAssoc a b c = Three a b c -> Three a b c -> Three a b c -> Bool

-- 5. 
data Four a b c d = Four a b c d deriving (Eq, Show)

instance (S.Semigroup a, S.Semigroup b, S.Semigroup c, S.Semigroup d)
  => S.Semigroup (Four a b c d) where
    Four x y z w <> Four x' y' z' w' = Four (x S.<> x') (y S.<> y') (z S.<> z') (w S.<> w')

instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d)
  => Arbitrary (Four a b c d) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    d <- arbitrary
    return (Four a b c d)

type FourAssoc a b c d = Four a b c d -> Four a b c d -> Four a b c d -> Bool

-- 6.
newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance S.Semigroup BoolConj where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Arbitrary BoolConj where
  arbitrary = do
    a <- arbitrary
    return (BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

-- 7.
newtype BoolDisj = BoolDisj Bool deriving (Eq, Show)

instance S.Semigroup BoolDisj where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _ = BoolDisj False

instance Arbitrary BoolDisj where
  arbitrary = do
    a <- arbitrary
    return (BoolDisj a)

type BoolDisjAssoc = BoolDisj -> BoolDisj -> BoolDisj -> Bool

-- 8.
data Or a b = Fst a | Snd b deriving (Eq, Show)

instance S.Semigroup (Or a b) where
  _ <> Snd x = Snd x
  Snd x <> _ = Snd x
  _ <> Fst x = Fst x

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Fst a)), (1, return (Snd b))]

type OrAssoc a b = Or a b -> Or a b -> Or a b -> Bool

-- 9. TODO >>>>>
newtype Combine a b = Combine { unCombine :: a -> b }

instance (S.Semigroup b) => S.Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f S.<> g)

instance Show (Combine a b) where
  show (Combine _) = "Combine instance"

-- fn f g = (const (\x' -> f x') 1 == const (\x' -> g x') 1)

-- instance Eq (Combine a b) where
--   (Combine f) == (Combine g) = fn f g

-- instance Eq (Combine a b) where
--   (Combine f) == (Combine g) = do
--     let a = (\x' -> f x')
--     let b = (\x' -> g x')
--     return $ (const a == const b)

-- Stolen on the Internet
genFunc :: (CoArbitrary a, Arbitrary b) => Gen (a -> b)
genFunc = arbitrary

genCombine :: (CoArbitrary a, Arbitrary b) => Gen (Combine a b)
genCombine = do
  f <- genFunc
  return $ Combine f

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = genCombine

type CombineAssoc a b = Combine a b -> Combine a b -> Combine a b -> Bool

-- 10. TODO >>>>>>
newtype Comp a = Comp { unComp :: a -> a }

instance (S.Semigroup a) => S.Semigroup (Comp a) where
  Comp f <> Comp g = Comp (f S.<> g)

-- f = Comp $ \n -> n + 1
-- g = Comp $ \n -> n - 1
-- x = unComp (f <> g) $ 0

-- 11.
data Validation a b =
  Failure' a | Success' b
  deriving (Eq, Show)

instance S.Semigroup a => S.Semigroup (Validation a b) where
  Failure' x <> Failure' y = Failure' (x S.<> y)
  Success' x <> _ = Success' x
  Failure' x <> Success' y = Success' y

instance (Arbitrary a, Arbitrary b) => Arbitrary (Validation a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (Failure' a)), (1, return (Success' b))]

type ValidationAssoc a b = Validation a b -> Validation a b -> Validation a b -> Bool

-- main = do
--   let failure :: String
--         -> Validation String Int
--       failure = Failure'
--       success :: Int
--         -> Validation String Int
--       success = Success'
--   print $ success 1 S.<> failure "blah"
--   print $ failure "woot" S.<> failure "blah"
--   print $ success 1 S.<> success 2
--   print $ failure "woot" S.<> success 2

-- Monoid exercises
-- 1.
instance Monoid Trivial where
  mempty = Trivial
  mappend = (S.<>)

checkTrivial :: IO ()
checkTrivial = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: TrivAssoc)
  quickCheck (mli :: Trivial -> Bool)
  quickCheck (mlr :: Trivial -> Bool)

-- 2.
instance (Monoid a, S.Semigroup a) => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (S.<>)

checkIdentity :: IO ()
checkIdentity = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: IdentAssoc String)
  quickCheck (mli :: Identity [Int] -> Bool)
  quickCheck (mlr :: Identity [Int] -> Bool)

-- 3.
instance Monoid BoolConj where
  mempty = BoolConj True
  mappend = (S.<>)

checkBoolConj :: IO ()
checkBoolConj = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: BoolConjAssoc)
  quickCheck (mli :: BoolConj -> Bool)
  quickCheck (mlr :: BoolConj -> Bool)

-- 4.
instance Monoid BoolDisj where
  mempty = BoolDisj False
  mappend = (S.<>)

checkBoolDisj :: IO ()
checkBoolDisj = do
  let sa = semigroupAssoc
      mli = monoidLeftIdentity
      mlr = monoidRightIdentity
  quickCheck (sa :: BoolDisjAssoc)
  quickCheck (mli :: BoolDisj -> Bool)
  quickCheck (mlr :: BoolDisj -> Bool)

-- 5. TODO >>>>>
instance (Monoid b, S.Semigroup b) => Monoid (Combine a b) where
  mempty = Combine { unCombine = mempty }
  mappend = (S.<>)

-- checkCombine :: IO ()
-- checkCombine = do
--   let sa = semigroupAssoc
--       mli = monoidLeftIdentity
--       mlr = monoidRightIdentity
--   quickCheck (sa :: CombineAssoc Int Int)
--   quickCheck (mli :: Combine Int Int -> Bool)
--   quickCheck (mlr :: Combine Int Int -> Bool)

-- 6. TODO >>>>
instance (Monoid a, S.Semigroup a) => Monoid (Comp a) where
  mempty = Comp { unComp = mempty }
  mappend = (S.<>)

-- 7.
newtype Mem s a = Mem { runMem :: s -> (a, s) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem { runMem = \x -> (mempty, x)}
  Mem { runMem = x } `mappend` Mem { runMem = y } =
    Mem { runMem = \x' -> (mappend (fst (x x')) (fst (y x')), snd (x (snd (y x')))) }

f' = Mem $ \s -> ("hi", s + 1)

checkMem = do
  let rmzero = runMem mempty 0
      rmleft = runMem (f' <> mempty) 0
      rmright = runMem (mempty <> f') 0
  print rmleft
  print rmright
  print (rmzero :: (String, Int))
  print $ rmleft == runMem f' 0
  print $ rmright == runMem f' 0

-- ("hi",1)
-- ("hi",1)
-- ("",0)
-- True
-- True

--- MAIN ---
  
main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
  quickCheck (semigroupAssoc :: TwoAssoc String String)
  quickCheck (semigroupAssoc :: ThreeAssoc String String String)
  quickCheck (semigroupAssoc :: FourAssoc String String String [Int])
  quickCheck (semigroupAssoc :: OrAssoc Int Int)
  -- quickCheck (semigroupAssoc :: CombineAssoc [Int] [Int])
  quickCheck (semigroupAssoc :: ValidationAssoc String Int)
  checkTrivial
  checkIdentity
  checkBoolConj
  checkBoolDisj
  -- checkCombine
  checkMem
