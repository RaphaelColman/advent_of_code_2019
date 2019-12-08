module AoC3.CrossedWiresSpec (spec) where

import Test.Hspec
import AoC3.CrossedWires

spec :: Spec
spec = do
  describe "Completion test" $ do
    it "Inputs should get correct output" $ do
      closestIntersection "D3,R3,U6\nR6" `shouldBe` Just 3
      closestIntersection "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` Just 135
      closestIntersection "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` Just 159
      closestIntersection "D3,R3,U6\nU2,R12\nR6" `shouldBe` Just 3
      closestIntersection "R8,U5,L5,D3\nU7,R6,D4,L4" `shouldBe` Just 6
      closestIntersection "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51" `shouldBe` Nothing

    it "should calculate all points covered by a line" $ do
      line ["R2", "D4", "L3"] `shouldBe` [(0,0), (1,0), (2,0), (2,-1), (2,-2), (2,-3), (2,-4), (1,-4),(0,-4),(-1,-4)]

    it "produces correct tuples from direction" $ do
      up 2 (0,0) `shouldBe` ([(0,1), (0,2)] :: [(Integer, Integer)])
      right 2 (1,0) `shouldBe` ([(2,0), (3,0)] :: [(Integer, Integer)])
      left 3 (2,2) `shouldBe` ([(1,2), (0,2), (-1,2)] :: [(Integer, Integer)])
      down 1 (0,0) `shouldBe` ([(0,-1)] :: [(Integer, Integer)])

    it "should calculate manhattan distance" $ do
      manhattan (3,4) (0,0) `shouldBe` 7
      manhattan (-4,-9) (0,0) `shouldBe` 13

    it "should find all crossings between two lines" $ do
      (crossings (line ["D3","R3","U6"]) (line ["R6"])) `shouldBe` [(3,0)]
      (crossings (line ["D3","R3","U6"]) (line ["L6"])) `shouldBe` []

    it "should find all crossings between a list of lines" $ do
      (allCrossings [(line ["D3","R3","U6"]),(line ["R6"])]) `shouldBe` [(3,0)]
      (allCrossings [(line ["D3","R3","U1", "L6"]),(line ["U6"])]) `shouldBe` []
      (allCrossings [(line ["D3","R3","U6"]),(line ["U6"]), (line ["R6"])]) `shouldBe` [(3,0)]
      (allCrossings [(line ["D3","R3","U6"]),(line ["U2", "R12"]), (line ["R6"])]) `shouldBe` [(3,2),(3,0)]

    it "should calculate steps for line to point" $ do
      stepsForLine (3,3) (line ["R8","U5","L5","D3"]) `shouldBe` Just 20
      stepsForLine (3,3) (line ["U7","R6","D4","L4"]) `shouldBe` Just 20

    it "should calculate total time" $ do
      time l1 l2 (3,3) `shouldBe` Just 40


    it "should calculate shortest intersection" $ do
      shortestIntersection "R98,U47,R26,D63,R33,U87,L62,D20,R33,U53,R51\nU98,R91,D20,R16,D67,R40,U7,R15,U6,R7" `shouldBe` Just 410
      shortestIntersection "R75,D30,R83,U83,L12,D49,R71,U7,L72\nU62,R66,U55,R34,D71,R55,D58,R83" `shouldBe` Just 610


l1 :: Line
l1 = line ["R8","U5","L5","D3"]

l2 :: Line
l2 = line ["U7","R6","D4","L4"]
