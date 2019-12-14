module AoC6.OrbitMap where

import qualified Data.Map.Lazy as Map
import Data.List.Split
import System.IO

main :: IO ()
main = do
  handle <- openFile "src/AoC6/input.txt" ReadMode  
  contents <- hGetContents handle
  print $ calculateOrbits $ parseOrbits contents

calculateOrbits :: Map.Map String String -> Int
calculateOrbits orbitMap = sum $ Map.map (calcOrbits . Just) orbitMap
    where calcOrbits :: Maybe String -> Int
          calcOrbits (Just child) = (+1) $ calcOrbits $ Map.lookup child orbitMap
          calcOrbits Nothing = 0

parseOrbit :: String -> (String, String)
parseOrbit orbitStr = let [child, parent] = splitOn ")" orbitStr in
    (parent, child)

--Function composition FTW!!!
parseOrbits :: String -> Map.Map String String
parseOrbits = Map.fromList . map parseOrbit . lines