main :: IO ()
main = do
    contents <- getContents
    putStrLn $ show $ sum (map fuelRequired (transform contents))

transform :: String -> [Int]
transform contents = map read (lines contents)

fuelRequired :: Int -> Int
fuelRequired fuel = (fuel `div` 3) - 2

--Tests--
one = fuelRequired 12 == 2
two = fuelRequired 14 == 2
three = fuelRequired 1969 == 654
four = fuelRequired 100756 == 33583
