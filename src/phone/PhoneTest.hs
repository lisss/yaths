module PhoneTest where

import Phone
import Test.Hspec

main :: IO ()
main = hspec $
  describe "coolestWord'" $ do
    it "no words" $
      coolestWord [] `shouldBe` ""
    it "one word" $
      coolestWord ["bear"] `shouldBe` "bear"
    it "convo" $
      coolestWord convo `shouldBe` "Lol"
    
