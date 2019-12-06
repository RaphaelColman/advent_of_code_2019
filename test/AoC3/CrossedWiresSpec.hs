module AoC3.CrossedWiresSpec (spec) where

import Test.Hspec
import AoC3.CrossedWires

spec :: Spec
spec = do
  describe "Completion test" $ do
    it "Inputs should get correct output" $ do
      closestIntersection "D3,R3,U6\nR6" `shouldBe` 3
      --closestIntersection "R75,D30,R83,U83,L12,D49,R71,U7,L72" `shouldBe` 159 -- This is clearly wrong, but I'm leaving it in because it proves that this line crosses itself
      --closestIntersection "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU99,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` 135
      --closestIntersection "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` 159

    it "should calculate all points covered by a line" $ do
      line ["R2", "D4", "L3"] `shouldBe` [(0,0), (1,0), (2,0), (2,-1), (2,-2), (2,-3), (2,-4), (1,-4),(0,-4),(-1,-4)]

    it "produces correct tuples from direction" $ do
      up 2 (0,0) `shouldBe` [(0,1), (0,2)]
      right 2 (1,0) `shouldBe` [(2,0), (3,0)]
      left 3 (2,2) `shouldBe` [(1,2), (0,2), (-1,2)]
      down 1 (0,0) `shouldBe` [(0,-1)]

    it "should calculate manhattan distance" $ do
      manhattan (3,4) (0,0) `shouldBe` 7
      manhattan (-4,-9) (0,0) `shouldBe` 13

    it "should find all crossings between two lines" $ do
      (crossings (line ["D3","R3","U6"]) (line ["R6"])) `shouldBe` [(3,0)]

    it "should find all crossigns between a list of lines" $ do
      (allCrossings [(line ["D3","R3","U6"]),(line ["R6"])]) `shouldBe` [[(3,0)]]

