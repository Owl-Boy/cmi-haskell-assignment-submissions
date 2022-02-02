-- Problem 1
nextSquare :: Integer -> Integer
nextSquare n
  | n <= 0 = 0
  | otherwise = ceiling (sqrt (fromIntegral n)) ^ 2

-- Problem 2
previousCube :: Integer -> Integer
previousCube n
  -- cannot use floor == ceiling as test because floor (1000^(1/3))
  -- is 9 and ceiling is 10 (floating point answers are not exact)
  | p ^ 3 == n = n
  | otherwise = (p - 1) ^ 3
  where
    p = ceiling (fromIntegral n ** (1 / 3))

-- Problem 3
-- this function is a modified version of what was
-- written during the lecture
fastRev :: Integer -> Integer
fastRev = (`revInto` 0)
  where
    revInto :: Integer -> Integer -> Integer
    revInto ip op
      | ip == 0 = op
      | otherwise = revInto q (10 * op + r)
      where
        (q, r) = ip `divMod` 10

nextPalin :: Integer -> Integer
nextPalin n
  | n < 0 = 0
  | n == fastRev n = n
  | otherwise = nextPalin (n + 1)

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn m n
  | n <= 1 || m > n = []
  -- n >= 2 in next line
  | m <= 1 = primesIn 2 n
  | otherwise =
    if isPrime m
      then m : primesIn (m + 1) n
      else primesIn (m + 1) n

-- Problem 5
twoUptoSqrtN :: Integer -> [Integer]
twoUptoSqrtN n = takeWhile (`squareLessEq` n) [2, 3 ..]
  where
    squareLessEq :: Integer -> Integer -> Bool
    squareLessEq p n = (p * p) <= n

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = null [i | i <- twoUptoSqrtN n, n `mod` i == 0]

-- Problem 6
decToBin :: Integer -> Integer
decToBin n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (n `mod` 2) + 10 * decToBin (n `div` 2)

-- Problem 7
binToDec :: Integer -> Integer
binToDec n
  | n == 0 = 0
  | n == 1 = 1
  | otherwise = (n `mod` 10) + 2 * binToDec (n `div` 10)
