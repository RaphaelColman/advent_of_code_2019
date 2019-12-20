module AoC10.MonitoringStationsSpec (spec) where


import Test.Hspec
import AoC10.MonitoringStations
import System.IO
import Linear.V2
import qualified Data.Set as Set


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
      canSee (V2 2 3) (V2 8 12) (Set.fromList [V2 4 6]) `shouldBe` False
      canSee (V2 2 3) (V2 8 12) (Set.fromList [V2 5 6]) `shouldBe` True
    
    it "can count visible asteroids" $
      ioCountVisible (testInput 1) (V2 3 4) `shouldReturn` 8

    it "can pick the best asteroids" $ do
      ioBestAsteroidScore (testInput 1) `shouldReturn` 8
      ioBestAsteroidScore (testInput 2) `shouldReturn` 33
      ioBestAsteroidScore (testInput 3) `shouldReturn` 35
      ioBestAsteroidScore (testInput 4) `shouldReturn` 41
      ioBestAsteroidScore (testInput 5) `shouldReturn` 210
    
    describe "Vaporization" $
      it "Can find the 200th asteroid to be vaporized" $ do
        ioVaporizeN (V2 11 13) (testInput 5) 2 `shouldReturn` V2 12 1
        ioVaporizeN (V2 11 13) (testInput 5) 3 `shouldReturn` V2 12 2
        ioVaporizeN (V2 11 13) (testInput 5) 10 `shouldReturn` V2 12 8
        ioVaporizeN (V2 11 13) (testInput 5) 20 `shouldReturn` V2 16 0
        ioVaporizeN (V2 11 13) (testInput 5) 200 `shouldReturn` V2 8 2

    describe "Sorting asteroids" $
      it "should sort asteroids" $
        sortAsteroids (V2 2 2) unsortedAsteroids `shouldBe` sortedAsteroids
      

ioParse :: IO String -> IO (Set.Set (V2 Int))
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

expectedCoordinates :: Asteroids
expectedCoordinates = Set.fromList $ map (uncurry V2) points 
  where points = [(1,0),(4,0),(0,2),(1,2),(2,2),(3,2),(4,2),(4,3),(3,4),(4,4)]
 
ioVaporizeN :: V2 Int -> IO String -> Int -> IO (V2 Int)
ioVaporizeN base input n = do 
  asteroids <- ioParse input
  pure $ vaporizeN base asteroids n

unsortedAsteroids :: [V2 Int]
unsortedAsteroids = map (uncurry V2) $ reverse [(3,4), (4,0), (0,2), (4,2), (1,0)]

sortedAsteroids :: [V2 Int]
sortedAsteroids = map (uncurry V2) [(4,0), (4,2), (3,4), (0,2), (1,0)]
