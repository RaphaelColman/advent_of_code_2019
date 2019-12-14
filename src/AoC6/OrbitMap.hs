module AoC6.OrbitMap where

import qualified Data.Map.Lazy as Map
import Data.List.Split
import System.IO
import qualified Data.Set as Set

main :: IO ()
main = do
  handle <- openFile "src/AoC6/input.txt" ReadMode  
  contents <- hGetContents handle
  print $ calculateOrbits $ parseOrbits contents
  print $ calculateShortestPath "YOU" "SAN" $ parseOrbits contents

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

calculateShortestPath :: String -> String -> Map.Map String String -> Int
calculateShortestPath you santa orbitMap = let youChildren = Set.fromList (getAllChildren you orbitMap);
                                                santaChildren = Set.fromList (getAllChildren santa orbitMap);
                                                justYourChildren = Set.size $ Set.difference youChildren santaChildren;
                                                justSantasChildren = Set.size $ Set.difference santaChildren youChildren in
                                                  justYourChildren + justSantasChildren


getAllChildren :: String -> Map.Map String String -> [String]
getAllChildren start orbitMap = case Map.lookup start orbitMap of
  Just child -> child : getAllChildren child orbitMap
  Nothing -> []