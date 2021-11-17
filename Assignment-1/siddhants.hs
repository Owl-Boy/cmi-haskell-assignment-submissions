-- Problem 1
nextSquare :: Integer -> Integer
nextSquare x 
    | x <= 0 = 0
    | otherwise = head [s ^ 2 | s <- [0..x], s ^ 2 >= x]

-- Problem 2
previousCube :: Integer -> Integer
previousCube x 
    | x <= 0 = 0
    | otherwise = last [s ^ 3 | s <- [0..x], s ^ 3 <= x]

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin x 
    | x < 0 = 0
    | x == read (reverse (show x)) = x
    | otherwise = nextPalin (x + 1)

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u
    | l > u = []
    | otherwise = [prime | prime <- [l..u], isPrime prime]

-- Problem 5
isPrime :: Integer -> Bool
isPrime x
    | x <= 0 = False
    | otherwise = length [factor | factor <- [1..x], x `mod` factor == 0] == 2

-- Problem 6
decToBin :: Integer -> Integer
decToBin x
    | x < 0 = error "Enter Positive Integer"
    | x == 0 = 0
    | otherwise = decToBin (x `div` 2) * 10 + x `mod` 2

-- Problem 7
binToDec :: Integer -> Integer
binToDec x
    | x < 0 = error "Enter Positive Integer"
    | x == 0 = 0
    | otherwise = binToDec (x `div` 10) * 2 + x `mod` 10
