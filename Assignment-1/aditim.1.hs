-- Problem 1
nextSquare :: Integer -> Integer
nextSquare n
    | n <= 0 = 0
    | otherwise = ceiling (sqrt (fromIntegral n)) ^ 2

-- Problem 2
previousCube :: Integer -> Integer
previousCube n = floor (fromIntegral (n+1) ** (1/3)) ^ 3

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin n
    | isPalin = n
    | otherwise = nextPalin (n+1)
  where
      isPalin = show n == reverse (show n)

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u
    | l > u = []
    | u <= 0 = []
    | otherwise = [x | x <- [l..u], isPrime x]


-- Problem 5
isPrime :: Integer -> Bool
isPrime n
    | n <= 0 || n == 1 = False
    | otherwise = null [x | x <- [2..(n-1)], n `mod` x  == 0]

-- Problem 6
decToBin :: Integer -> Integer
decToBin n
    | n == 0 = 0
    | otherwise = n `mod` 2 + 10 * decToBin (n `div` 2)

-- Problem 7
binToDec :: Integer -> Integer
binToDec n
    | n == 0 = 0
    | otherwise = n `mod` 10 + 2 * binToDec (n `div` 10)