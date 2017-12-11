module Test where

-- import Hangman(Puzzle, fillInCharacter, handleGuess)
import Hangman
import Test.QuickCheck
import Test.Hspec

-- pzGen :: Gen (String -> Puzzle)
-- pzGen = do
--   a <- arbitrary :: Gen String
--   b <- listOf (arbitrary :: Gen (Maybe Char))
--   return (Puzzle a b)

-- TODO: use gen?
main :: IO ()
main = hspec $ do
  describe "fillInCharacter" $ do
    it "fresh puzzle" $ do
      let pzl = freshPuzzle "liss"
      let pzl1 = Puzzle "liss" (map (\x' -> Nothing) "liss") "a"
      fillInCharacter pzl 'a' `shouldBe` pzl1
    it "filled in puzzle" $ do
      let pzl = Puzzle "liss" [Just 'b'] []
      let pzl1 = Puzzle "liss" [Just 'b'] "b"
      fillInCharacter pzl 'b' `shouldBe` pzl1
    it "guessed multiple" $ do
      let pzl = Puzzle "liss" [Just 'b'] []
      let pzl1 = Puzzle "liss" [Just 'b'] "bx"
      fillInCharacter (fillInCharacter pzl 'x') 'b' `shouldBe` pzl1
  describe "handleGuess" $ do
    it "fresh puzzle" $ do
      let pzl = freshPuzzle "liss"
      let pzl1 = Puzzle "liss" (map (\x' -> Nothing) "liss") "a"
      res <- handleGuess pzl 'a'
      res `shouldBe` pzl1
    it "guessed" $ do
      let pzl = freshPuzzle "liss"
      let pzl1 = Puzzle "liss" (map (\x -> Nothing) "li" ++ map (\x -> Just x) "ss") "s"
      res <- handleGuess pzl 's'
      res `shouldBe` pzl1

