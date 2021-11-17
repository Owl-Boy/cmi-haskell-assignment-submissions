--Problem 1
nextSquare :: Integer -> Integer
nextSquare n = checkSquare n 1 where
    checkSquare n i 
        | n > i^2 && n <=(i+1)^2 = i^2
        | otherwise = checkSquare n (i+1)

--Problem 2
previousCube :: Integer->Integer
previousCube n  = checkCube n 1 where
    checkCube n i 
        | n >= i^3 && n< (i+1)^3 = i^3
        | otherwise = checkCube n (i+1) 


-- Problem 3
nextPalin :: Integer -> Integer
nextPalin n 
    | n < 0 = 0
    | ifPalin n = n 
    | otherwise = nextPalin (n+1)

ifPalin :: Integer -> Bool
ifPalin n
    | n<10 && n>=0 = True
    | n == revNum 0 n = True
    | otherwise = False

revNum :: Integer -> Integer->Integer
revNum n 0 = n
revNum n k = revNum (n*10 + (mod k 10))  (div k 10)



-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u 
    | l > u  = []
    | u < 0 = []
    | l < 0 = primesIn 0 u 
    | isPrime l = l:(primesIn (l+1) u)
    | otherwise = primesIn (l+1) u

--Problem5
isPrime :: Integer->Bool
isPrime p 
    | p <= 1 = False 
    | otherwise = null [ x | x<-[2..floor(sqrt(fromIntegral p))], p `mod` x == 0]


-- Problem 6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin 1 = 1
decToBin n = 10 * decToBin (div n 2) +  (mod n 2)


-- Problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0 
binToDec 1 = 1
binToDec n = (mod n 10) + (binToDec (div n 10)) * 2