module AoC1.RocketEquation where

--main :: IO ()
--main = do
--    contents <- getContents
--    putStrLn $ show $ sum (map fuelRequiredSimple (transform contents))
--    putStrLn $ show $ sum (map fuelRequired (transform contents))

transform :: String -> [Int]
transform contents = map read (lines contents)

fuelRequiredSimple :: Int -> Int
fuelRequiredSimple x = (x `div` 3) - 2

fuelRequired :: Int -> Int
fuelRequired x = go x 0
  where go x total
          | fuelRequiredSimple x <= 0 = total
          | otherwise = go (fuelRequiredSimple x) (fuelRequiredSimple  x + total)

