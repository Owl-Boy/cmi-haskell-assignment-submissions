-- Question 1

nextSquare :: Integer -> Integer
nextSquare a = ceiling (sqrt (fromIntegral a)) ^ 2

-- Question 2

previousCube :: Integer -> Integer
previousCube a = cubeCheck 1 a
  where
    cubeCheck :: Integer -> Integer -> Integer
    cubeCheck x a =
      if (x + 1) ^ 3 > a
        then x ^ 3
        else cubeCheck (x + 1) a

-- Question 3

numLen :: Integer -> Integer
numLen x
  | x == 0 = 0
  | otherwise = 1 + numLen (x `div` 10)

isPalin :: Integer -> Integer -> Bool
isPalin num len
  | len <= 1 = True
  | otherwise = mod num 10 == (num `div` (10 ^ (len - 1))) && isPalin (num `mod` (10 ^ (len - 1)) `div` 10) (len - 2)

nextPalin :: Integer -> Integer
nextPalin num =
  if isPalin num (numLen num)
    then num
    else nextPalin (num + 1)

-- Question 5

isPrime :: Integer -> Bool
isPrime num
  | num == 1 = False
  | num < 4 = True
  | otherwise = helper 2 num
  where
    helper :: Integer -> Integer -> Bool
    helper a b
      | a > floor (sqrt (fromIntegral b)) = True
      | otherwise = b `mod` a /= 0 && helper (a + 1) b

-- Question 4

primesIn :: Integer -> Integer -> [Integer]
primesIn a b
  | a > b = []
  | otherwise = [a | isPrime a] ++ primesIn (a + 1) b

-- Question 6

decToBin :: Integer -> Integer
decToBin x = helper x 0
  where
    helper :: Integer -> Integer -> Integer
    helper n m
      | n == 0 = 0
      | otherwise = (if n `mod` 2 == 1 then 10 ^ m else 0) + helper (n `div` 2) (m + 1)

-- Question 7

binToDec :: Integer -> Integer
binToDec x = helper x 0
  where
    helper :: Integer -> Integer -> Integer
    helper n m
      | n == 0 = 0
      | otherwise = (if n `mod` 10 == 1 then 2 ^ m else 0) + helper (n `div` 10) (m + 1)
