{-# LANGUAGE TemplateHaskell #-}
module AoC12.NBody where

import Linear.V3
import Linear.Vector
import Control.Lens
import Control.Lens.TH
import Data.List
import Common.Utils
import Data.Char(isDigit)
import Text.Read(readMaybe)
import System.IO
import Debug.Trace
import qualified Data.Set as Set
import Data.Set(Set(..))

type Position = V3 Int
type Velocity = V3 Int

data Moon = Moon {
    _position :: Position,
    _velocity :: Velocity
    } deriving (Eq, Show, Ord)

type System = [Moon]

makeLenses ''Moon

main :: IO ()
main = do
    fileHandle <- openFile "src/AoC12/input.txt" ReadMode
    contents <- hGetContents fileHandle
    let system = parseSystem contents
    print $ totalEnergy 1000 <$> system 
    print $ calculateFirstRepeat <$> system

totalEnergy :: Int -> System -> Int
totalEnergy n = energy . last . take (n+1) . iterate step

calcVelocity :: Moon -> Moon -> Velocity
calcVelocity this other = diffV
    where diffV = convertOrd <$> ((other ^. position) - (this ^. position))
          convertOrd x
            | x > 0 = 1
            | x < 0 = -1
            | otherwise = 0

calcVelocities :: Moon -> System -> Velocity
calcVelocities m = sumV . map (calcVelocity m)

step :: System -> System
step sys = newSys 
    where newSys = map (applyVelocity . (\m -> m & velocity %~ (\v -> v + calcVelocities m sys))) sys
          applyVelocity m = m & position %~ (\x -> x + m ^. velocity)

energy :: System -> Int
energy = sum . map moonEnergy

moonEnergy :: Moon -> Int
moonEnergy moon = pot * kin
    where pot = foldrV3 ((+) . abs) 0 $ moon ^. position
          kin = foldrV3 ((+) . abs) 0 $ moon ^. velocity


parseSystem :: String -> Maybe System
parseSystem = mapM parseMoon . lines

parseMoon :: String -> Maybe Moon
parseMoon str = do 
    [x,y,z] <- traverse readMaybe $ words $ clearCharacters p str
    pure $ Moon (V3 x y z) (V3 0 0 0)
    where p c
            | c == '-' = False 
            | otherwise = not $ isDigit c

nAxis :: ((b -> Const b b) -> V3 Int -> Const b (V3 Int)) -> [Moon] -> ([b], [b])
nAxis f sys = (map xPos sys, map xVel sys)
    where xPos = view f ._position
          xVel = view f ._velocity

nPhase :: Eq b => ((b -> Const b b) -> V3 Int -> Const b (V3 Int)) -> [Moon] -> Int
nPhase f sys = ((+) 1 . length . takeWhile (\s -> nAxis f s /= nAxis f sys) . iterate step) (step sys) 

calculateFirstRepeat :: System -> Int
calculateFirstRepeat sys = lcm (nPhase _z sys) (lcm (nPhase _y sys) (nPhase _x sys))