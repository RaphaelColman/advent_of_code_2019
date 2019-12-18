module AoC10.MonitoringStationsSpec (spec) where


import Test.Hspec
import AoC10.MonitoringStations
import System.IO
import Linear.V2


spec :: Spec
spec = do
  describe "Reading the input file" $
    it "can convert the input to a list of vectors" $
        ioParse (testInput 1) `shouldReturn` expectedCoordinates 
  
  describe "Handling vector spaces" $
    it "can find the points between two points" $ do
      pointsBetween (V2 0 0) (V2 8 12) `shouldBe` map (uncurry V2) [(2,3), (4,6), (6,9)]
      pointsBetween (V2 0 0) (V2 (-8) (-12)) `shouldBe` map (uncurry V2) [(-2,-3), (-4,-6), (-6,-9)]
      pointsBetween (V2 0 0) (V2 0 (-2)) `shouldBe` map (uncurry V2) [(0,-1)]
  
  describe "Incorporating the asteroid map" $ do
    it "can tell if two asteroids can see each other" $ do
      canSee (V2 2 3) (V2 8 12) [V2 4 6] `shouldBe` False
      canSee (V2 2 3) (V2 8 12) [V2 5 6] `shouldBe` True
    
    it "can count visible asteroids" $
      ioCountVisible (testInput 1) (V2 3 4) `shouldReturn` 8

    it "can pick the best asteroids" $ do
      ioBestAsteroidScore (testInput 1) `shouldReturn` 8
      ioBestAsteroidScore (testInput 2) `shouldReturn` 33
      ioBestAsteroidScore (testInput 3) `shouldReturn` 35
      ioBestAsteroidScore (testInput 4) `shouldReturn` 41
      ioBestAsteroidScore (testInput 5) `shouldReturn` 210


ioParse :: IO String -> IO [V2 Int] 
ioParse str = parseInput <$> str

ioBestAsteroidScore :: IO String -> IO Int
ioBestAsteroidScore input = do
  asteroids <- ioParse input
  pure $ bestAsteroidScore asteroids

ioCountVisible :: IO String -> V2 Int -> IO Int
ioCountVisible input a = do 
  asteroids <- ioParse input
  pure $ countVisible a asteroids

testInput :: Int -> IO String
testInput i = do
  handle <- openFile ("test/AoC10/input" ++ show i ++ ".txt") ReadMode
  hGetContents handle

expectedCoordinates :: [V2 Int]
expectedCoordinates = map (uncurry V2) points 
  where points = [(1,0),(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(3,4),(4,4)]
 