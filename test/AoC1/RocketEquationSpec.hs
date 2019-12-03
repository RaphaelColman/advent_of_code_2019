module AoC1.RocketEquationSpec (spec) where

import Test.Hspec
import AoC1.RocketEquation

spec :: Spec
spec = do
  describe "FuelRequiredSimple" $ do
    it "FuelRequiredSimple should calculate correctly" $ do
      fuelRequiredSimple 12 `shouldBe` 2
      fuelRequiredSimple 14 `shouldBe` 2
      fuelRequiredSimple 1969 `shouldBe` 654
      fuelRequiredSimple 100756 `shouldBe` 33583

    it "FuelRequired should include fuel calculation" $ do
      fuelRequired 12 `shouldBe` 2
      fuelRequired 14 `shouldBe` 2
      fuelRequired 1969 `shouldBe` 966
      fuelRequired 100756 `shouldBe` 50346
