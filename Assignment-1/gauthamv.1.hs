-- Problem 1
nextSquare :: Integer -> Integer
nextSquare n
    | n <= 0 = 0
    | n > 0 = ceiling (sqrt (fromIntegral n))^2

-- Problem 2
previousCube :: Integer -> Integer
previousCube n = floor ( (fromIntegral n) ** (1/3) )^3

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin n
    | n <= 0 = 0
    | n > 0 = if isPalin n then n else nextPalin (n + 1)

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u = [p | p <- [l..u], isPrime p]

-- Problem 5
isPrime :: Integer -> Bool
isPrime n = length (listOfFactors n) == 0

-- Problem 6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin n = (n `mod` 2) + 10 * decToBin (n `div` 2)

-- Problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec n = (n `mod` 10) + 2 * binToDec (n `div` 10)


-- AUXILIARY FUNCTIONS --


numDigits :: Integer -> Integer
numDigits n
    | n < 0 = error "We are assuming only nonnegative inputs for the time being."
    | n <= 9 = 1
    | n > 9 = 1 + numDigits (n `div` 10)

revNum :: Integer -> Integer
revNum n 
    | n < 0 = error "Only nonnegative integers can be reversed."
    | n <= 9 = n
    | n > 9 =  (n `mod` 10) * (10^( (numDigits n) - 1 )) + revNum (n `div` 10)

isPalin :: Integer -> Bool
isPalin n
    | n < 0 = False
    | n >= 0 = n == revNum n

listOfFactors :: Integer -> [Integer] -- lists all factors of n less than or equal to the square root of n
listOfFactors n = [x | x <- [2..ceiling (sqrt (fromIntegral n))], n `mod` x == 0]
