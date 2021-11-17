-- Problem 1
nextSquare :: Integer -> Integer
nextSquare n
  | n <= 0      = 0
  | otherwise   = isValid 1
    where
        isValid k = if k^2 >= n then k^2 else isValid (k+1)

-- Problem 2
previousCube :: Integer -> Integer
previousCube n = isValid 1
  where
      isValid k = if k^3 <= n then isValid (k+1) else (k-1)^3

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin n = if isPalin then n else nextPalin (n+1)
  where
      isPalin = (show n) == reverse (show n)

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn low high = filter isPrime [low..high]

-- Problem 5
isPrime :: Integer -> Bool
isPrime n
  | n <= 1      = False
  | otherwise   = not (True `elem` [n `mod` i == 0 | i <- toSqrt])
  where
      toSqrt = [2..round (sqrt (fromIntegral n))]


-- Problem 6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin n = n `mod` 2 + 10 * decToBin (n `div` 2)

-- Problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec n = n `mod` 10 + 2 * binToDec (n `div` 10)
