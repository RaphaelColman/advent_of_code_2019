module AoC10.MonitoringStationsSpec (spec) where


import Test.Hspec
import AoC10.MonitoringStations
import System.IO
import Common.Utils


spec :: Spec
spec =  
  describe "Placeholder" $
    it "Placeholder" $
        parseInput (testInput 1) `shouldReturn` [((1,1), 'h')] 

parseInput :: IO String -> IO [((Int, Int), Char)]
parseInput str = enumerateMultilineString <$> str

testInput :: Int -> IO String
testInput i = do
  handle <- openFile ("test/AoC10/input" ++ show i ++ ".txt") ReadMode
  hGetContents handle


testInput1 :: IO String
testInput1 = do
  handle <- openFile "test/AoC10/input1.txt" ReadMode
  hGetContents handle