-- Problem 1

checkgreatersq :: Integer -> Integer -> Integer
checkgreatersq a b
    | a^2 < b     = checkgreatersq (a+1) b
    | otherwise     = a^2
nextSquare :: Integer -> Integer
nextSquare a
    | a <= 0     = 0
    | otherwise  = checkgreatersq 0 a


-- Problem 2
previousCube :: Integer -> Integer
previousCube a
    | a < 0      = checknegcube 0 a
    | otherwise  = checkposcube 0 a

checknegcube :: Integer -> Integer -> Integer
checknegcube a b
    | a^3 > b     = checknegcube (a-1) b
    | otherwise    = a^3
checkposcube :: Integer -> Integer -> Integer
checkposcube a b
    | a^3 <= b     = checkposcube (a+1) b
    | otherwise    = (a-1)^3

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin a
    | a <= 0          = 0
    | checkpalin a    = a 
    | otherwise       = nextPalin (a + 1)

numlen :: Integer -> Integer
numlen n
    | n < 10    = 1
    | otherwise = 1 + numlen (n `div` 10)
revnum :: Integer -> Integer
revnum a
    | a < 10     = a
    | otherwise  = revnum (a `div` 10) + (a `mod` 10)*(10^(numlen a - 1))
checkpalin :: Integer -> Bool
checkpalin a = a == revnum a

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn a b 
    | a > b                 = []
    | b <= 0                = []
    | (a <= 0) && (b > 0)   = [x | x <- [0..b], isPrime x]
    | otherwise             = [x | x <- [a..b], isPrime x]

-- Problem 5
isPrime :: Integer -> Bool 
isPrime a
    | a < 1     = error "Invalid Integer"
isPrime 2 = True
isPrime a = checkempty (factor a)

checkempty :: [Integer] -> Bool
checkempty [] = True
checkempty (x:_) = False

factor :: Integer -> [Integer]
factor a = [x | x <- [2..(a-1)],a `mod` x == 0]

-- Problem 6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin a = 10 * decToBin (a `div` 2) + (a `mod` 2)  

-- Problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec a = 2 * binToDec (a `div` 10) + (a `mod` 10)
