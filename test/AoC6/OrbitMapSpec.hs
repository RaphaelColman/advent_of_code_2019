module AoC6.OrbitMapSpec (spec) where

import Test.Hspec
import AoC6.OrbitMap
import qualified Data.Map as Map
import qualified Data.Set as Set


spec :: Spec
spec =  do
  describe "Tying the knot" $
    it "Can calculate orbits from map" $ do
      calculateOrbits simpleMap `shouldBe` 3
      calculateOrbits complexMap `shouldBe` 42
    
  describe "Parsing an orbit string" $ 
    it "can turn an orbit string into a tuple" $
      parseOrbit "child)parent" `shouldBe` ("parent", "child")
    
  describe "Calculating the shortest distance" $
      it "Can calculate the shortest distance between two points" $
        calculateShortestPath "YOU" "SAN" complexMapWithYouAndSanta `shouldBe` 4


  


simpleMap :: Map.Map String String 
simpleMap = Map.fromList [("B", "Com"), ("C", "B")]

complexMap :: Map.Map String String
complexMap = Map.fromList [("B","COM"), ("C","B"), ("D","C"), ("E","D"), ("F","E"), ("G","B"), ("H","G"), ("I","D"), ("J","E"), ("K","J"), ("L","K")]

complexMapWithYouAndSanta :: Map.Map String String
complexMapWithYouAndSanta = Map.fromList [("B","COM"), ("C","B"), ("D","C"), ("E","D"), ("F","E"), ("G","B"), ("H","G"), ("I","D"), ("J","E"), ("K","J"), ("L","K"), ("YOU", "K"), ("SAN", "I")]