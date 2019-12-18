module AoC10.MonitoringStations where

import qualified Linear.V2 as V2
import Linear.V2(V2(..))
import Common.Utils
import qualified Data.Set as Set
import System.IO

type Asteroids = [V2 Int]

main :: IO()
main = do
    fileHandle <- openFile "src/AoC10/input.txt" ReadMode
    contents <- hGetContents fileHandle
    print $ bestAsteroidScore $ parseInput contents


parseInput :: String -> Asteroids 
parseInput = map (uncurry V2 . fst) . filter isAsteroid . enumerateMultilineString
    where isAsteroid tup = snd tup == '#' 


bestAsteroidScore :: Asteroids -> Int
bestAsteroidScore asteroids = maximum $ map (`countVisible` asteroids) asteroids

vSimplify :: V2 Int -> V2 Int
vSimplify (V2 x y) = V2 (x `div` gcd') (y `div` gcd')
    where gcd' = gcd x y

pointsBetween :: V2 Int -> V2 Int -> [V2 Int]
pointsBetween start finish = go start finish 
    where go start' finish'
            | start' == finish' = []
            | start' + step == finish = []
            | otherwise = let nextStep = start' + step in nextStep : go nextStep finish'
          step = vSimplify (finish - start)

canSee :: V2 Int -> V2 Int -> Asteroids -> Bool
canSee a1 a2 asteroids
    | a1 == a2 = False
    | otherwise = null common
    where pb = Set.fromList $ pointsBetween a1 a2
          common = Set.intersection pb $ Set.fromList asteroids

countVisible :: V2 Int -> Asteroids -> Int
countVisible x asteroids = length $ filter (\a -> canSee x a asteroids) asteroids
