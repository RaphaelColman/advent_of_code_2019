main :: IO ()
main = do
    contents <- getContents
    putStrLn $ show $ sum (map fuelRequiredSimple (transform contents))
    putStrLn $ show $ sum (map fuelRequired (transform contents))

transform :: String -> [Int]
transform contents = map read (lines contents)

fuelRequiredSimple :: Int -> Int
fuelRequiredSimple x = (x `div` 3) - 2

fuelRequired :: Int -> Int
fuelRequired x = go x 0
  where go x total
          | fuelRequiredSimple x <= 0 = total
          | otherwise = go (fuelRequiredSimple x) (fuelRequiredSimple  x + total)

--Tests--
one = fuelRequiredSimple 12 == 2
two = fuelRequiredSimple 14 == 2
three = fuelRequiredSimple 1969 == 654
four = fuelRequiredSimple 100756 == 33583

--part 2--
one_2 = fuelRequired 12 == 2
two_2 = fuelRequired 14 == 2
three_2 = fuelRequired 1969 == 966
four_2 = fuelRequired 100756 == 50346
passed_2 = and [one_2, two_2, three_2, four_2]
