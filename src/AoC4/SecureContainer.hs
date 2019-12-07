module AoC4.SecureContainer where

import Data.List

onlyAscendingDigits :: String -> Bool
onlyAscendingDigits i = go i True
  where go (x:y:xs) True = go (y:xs) (y >= x)
        go _ False = False
        go (_:[]) b = b
        go [] b = b

hasDigitPair :: String -> Bool
hasDigitPair = any (\(x,y) -> x == y) . consecs 

hasSoloDigitPair :: String -> Bool
hasSoloDigitPair = (>=1)
                  . length
                  . filter (\x -> (length x == 2))
                  . group

--I've nabbed this from an existing solution. Don't judge me!
consecs :: [a] -> [(a,a)]
consecs xs = zip xs (tail xs)

doFilter :: [Integer] -> Int
doFilter = length
          . filter (\x -> onlyAscendingDigits (show x) && hasDigitPair (show x))

doFilterPt2 :: [Integer] -> Int
doFilterPt2 = length
            . filter (\x -> onlyAscendingDigits (show x) && hasSoloDigitPair (show x))
