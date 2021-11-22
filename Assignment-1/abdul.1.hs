-- Problem 1
nextSquare :: Integer -> Integer
nextSquare n
  | n < 0 = 0
  | otherwise = (^2) . ceiling . sqrt . fromIntegral $ n

-- Problem 2
previousCube :: Integer -> Integer
previousCube = (^3) . floor . (flip (**) $ 1/3) . fromIntegral . succ

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin n = head $ filter isPalin [n,n+1..] where 
  isPalin p = (reverse $ show p) == show p

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u = filter isPrime [l..u]

-- Problem 5
isPrime :: Integer -> Bool
isPrime n 
  | n <= 0 = False
  | n==2 = True
  | otherwise = all (/= 0) $ map (mod n) [2..ceiling $ sqrt $ fromIntegral n]

-- Problem 6
helper :: Integer -> [Integer]
helper 0 = [0]
helper n = helper (div n 2) ++ [mod n 2]

decToBin :: Integer -> Integer
decToBin n = sum $ zipWith (*) (map (10^) [0..]) $ reverse . helper $ n

-- Problem 7
binToDec :: Integer -> Integer
binToDec b = sum $ zipWith (*) (map (2^) [0..]) (reverse . map (read . return) . show $ b)
