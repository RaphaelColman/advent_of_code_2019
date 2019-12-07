module AoC4.SecureContainerSpec (spec) where

import Test.Hspec
import AoC4.SecureContainer

spec :: Spec
spec = do
  describe "Ascending digits tests" $ do
    it "returns true if digits are ascending" $ do
      onlyAscendingDigits "55678" `shouldBe` True
      onlyAscendingDigits "111111" `shouldBe` True

    it "returns false if digits are not ascending" $ do
      onlyAscendingDigits "774" `shouldBe` False
      onlyAscendingDigits "7787" `shouldBe` False

  describe "Digit pair tests" $ do
    it "returns true if there is a digit pair" $ do
      hasDigitPair "4556" `shouldBe` True
      --hasDigitPair "9999" `shouldBe` True
      --hasDigitPair "12345699" `shouldBe` True

    it "returns false if there is no digit pair" $ do
      hasDigitPair "67890" `shouldBe` False
      hasDigitPair "8765" `shouldBe` False
