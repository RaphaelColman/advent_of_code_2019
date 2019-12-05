module AoC3.CrossedWiresSpec (spec) where

import Test.Hspec
import AoC3.CrossedWires

spec :: Spec
spec = do
  describe "Completion test" $ do
    xit "Inputs should get correct output" $ do
      --Should newlines become spaces or some other delimiter?
      closestIntersection "R75,D30,R83,U83,L12,D49,R71,U7,L72 U62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` 159
      closestIntersection "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51 U99,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` 135

    --it "should calculate all points covered by a line" $ do
    --  allPointsFromLine Line ["R2", "D4", "L3"] `shouldBe` [(0,0), (0,1), (-1, 1), (-2, 1), (-3,1), (-4,1), (-4,0), (-3,0), (-2,0)]

    it "produces correct tuples from direction" $ do
      up 2 (0,0) `shouldBe` [(0,1), (0,2)]
      right 2 (1,0) `shouldBe` [(2,0), (3,0)]
      left 3 (2,2) `shouldBe` [(1,2), (0,2), (-1,2)]
      down 1 (0,0) `shouldBe` [(0,-1)]


