module Common.Geometry where

import Linear.V2
import Linear.Vector

vLength :: Floating a => V2 a -> a
vLength (V2 x y) = sqrt (x^2 + y^2)

vNormalise :: Floating a => V2 a -> V2 a
vNormalise v = v ^/ vLength v

angleFromY :: RealFloat a => V2 Int -> a
angleFromY (V2 x y) = let theta = atan2 (fromIntegral x) (fromIntegral y) in
    if theta < 0 then theta + 2*pi else theta
