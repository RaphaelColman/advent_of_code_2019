{-# LANGUAGE TemplateHaskell #-}
module AoC12.NBody where

import Linear.V3
import Linear.Vector
import Control.Lens
import Control.Lens.TH
import Data.List
import Common.Utils

type Position = V3 Int
type Velocity = V3 Int

data Moon = Moon {
    _position :: Position,
    _velocity :: Velocity
    } deriving (Eq, Show)

type System = [Moon]

makeLenses ''Moon

main :: IO ()
main = print $ totalEnergy 1000 system

totalEnergy :: Int -> System -> Int
totalEnergy n = energy . last . take (n+1) . iterate step

calcVelocity :: Moon -> Moon -> Velocity
calcVelocity this other = diffV
    where originalVelocity = this ^. velocity
          diffV = convertOrd <$> ((other ^. position) - (this ^. position))
          convertOrd x
            | x > 0 = 1
            | x < 0 = -1
            | otherwise = 0

calcVelocities :: Moon -> System -> Velocity
calcVelocities m = (+) (m ^. velocity) . (sumV . map (calcVelocity m)) 
--This is crap. calcVelocity returns just the velocities calculated, whereas calcVelcoties incorporates the moon's original velocity
--This should just return the velocities calculated and leave it to the client to add to the original velocity

step :: System -> System
step sys = map (applyVelocity . (\m -> m & velocity .~ calcVelocities m sys)) sys
    where applyVelocity m = m & position %~ (\x -> x + m ^. velocity)

stepN :: System -> [System]
stepN = unfoldr doStep
    where doStep :: System -> Maybe (System, System)
          doStep sys' = let newSys = step sys' in Just (newSys, newSys)

energy :: System -> Int
energy = sum . map moonEnergy

moonEnergy :: Moon -> Int
moonEnergy moon = pot * kin
    where pot = foldrV3 ((+) . abs) 0 $ moon ^. position
          kin = foldrV3 ((+) . abs) 0 $ moon ^. velocity

inputMoon1 = Moon (V3( -4)(  3)(15)) (V3 0 0 0)
inputMoon2 = Moon (V3(-11)(-10)(13)) (V3 0 0 0)
inputMoon3 = Moon (V3(  2)(  2)(18)) (V3 0 0 0)
inputMoon4 = Moon (V3(  7)( -1)( 0)) (V3 0 0 0)

system = [inputMoon1, inputMoon2, inputMoon3, inputMoon4]