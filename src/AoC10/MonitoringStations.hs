module AoC10.MonitoringStations where

import qualified Linear.V2 as V2
import Linear.V2(V2(..))
import Common.Utils
import qualified Data.Set as Set
import Data.Set(Set(..))
import System.IO
import Common.Geometry
import Data.List

type Asteroids = Set (V2 Int)

main :: IO()
main = do
    fileHandle <- openFile "src/AoC10/input.txt" ReadMode
    contents <- hGetContents fileHandle
    print $ bestAsteroidScore' $ parseInput contents
    print $ vaporizeN (V2 26 29) (parseInput contents) 200


parseInput :: String -> Asteroids 
parseInput = Set.fromList . map (uncurry V2 . fst) . filter isAsteroid . enumerateMultilineString
    where isAsteroid tup = snd tup == '#' 


bestAsteroidScore :: Asteroids -> Int
bestAsteroidScore asteroids = maximum $ Set.map (`countVisible` asteroids) asteroids

bestAsteroidScore' :: Asteroids -> (V2 Int, Int)
bestAsteroidScore' asteroids = maximumBy (\x y -> compare (snd x) (snd y)) $ Set.map (\a -> (a, countVisible a asteroids)) asteroids

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
          common = Set.intersection pb asteroids

countVisible :: V2 Int -> Asteroids -> Int
countVisible x asteroids = length $ Set.filter (\a -> canSee x a asteroids) asteroids

vaporizeN :: V2 Int -> Asteroids -> Int -> V2 Int
vaporizeN base asteroids n = go base asteroids 0
    where go base' asteroids' destroyed
            | length visible > numToGo = let orderedAsteroids = sortAsteroids base' (Set.toList visible) in orderedAsteroids !! (numToGo - 1)
            | otherwise = go base' (Set.difference asteroids' visible) (length visible)
            where visible = Set.filter (\a -> canSee base' a asteroids') asteroids'
                  numToGo = n - destroyed


sortAsteroids :: V2 Int -> [V2 Int] -> [V2 Int]
sortAsteroids base = sortOn angleFromBase 
    where angleFromBase :: V2 Int -> Double
          angleFromBase a = angleFromY $ flipY (a - base)


--You have to do this because of the weird geometry (+y means going south etc)
flipY :: V2 Int -> V2 Int 
flipY (V2 x y) = V2 x (negate y)