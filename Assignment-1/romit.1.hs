-- Problem 1
nextSquare :: Integer -> Integer
nextSquare x
    | x < 0 = 0
    | otherwise = ceiling (sqrt (fromIntegral x))^2



-- Problem 2
previousCube :: Integer -> Integer
previousCube x
    | x <= 0 = error "Positive only"
    | otherwise = floor ((fromInteger x + 0.5) ** (1/3))^3


-- Problem 3
nextPalin :: Integer -> Integer
nextPalin x
    | x < 0 = 0
    | revnum x == x = x
    | otherwise = nextPalin (x + 1)
        where
        revnum :: Integer -> Integer
        revnum = into 0
            where
            into p 0 = p
            into p n = into (10 * p + n `mod` 10) (n `div` 10)



--Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l p
    | l <= p = if isPrime l then l : primesIn (l+1) p else primesIn (l+1) p
    | otherwise = []




-- Problem 5
isPrime :: Integer -> Bool
isPrime x = fac x == 2 where
    fac :: Integer -> Integer
    fac 1 = 1
    fac y
        | y <= 0 = 0
        | otherwise = fac (y - 1) + (if x `mod` y == 0 then 1 else 0)



-- Problem 6
decToBin :: Integer -> Integer
decToBin n
    | n < 0 = error "Non-negative input only"
    | otherwise = recBin 0 n where
        recBin _ 0 = 0 
        recBin a b = 10 ^ a * q + recBin (a+1) p where
            (p, q) =  b `divMod` 2





-- Problem 7
binToDec :: Integer -> Integer
binToDec n
    | n < 0 = error "Non-negative input only"
    | otherwise = recDec 0 n where
        recDec _ 0 = 0 
        recDec a b = 2 ^ a * q + recDec (a+1) p where
            (p, q) =  b `divMod` 10
