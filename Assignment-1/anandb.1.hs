-- Problem 1
issquare :: Integer -> Integer -> Bool
issquare n k 
 | ((n == 0) || (n == 1)) = True
 | (k^2 < n) = issquare n (k+1)
 | (k^2 == n) = True
 | (k^2 > n) = False
 | otherwise = False 

nextSquare :: Integer -> Integer 
nextSquare n 
 | (issquare n 1 == True) = n
 | otherwise = nextSquare (n+1)

-- Problem 2
iscube :: Integer -> Integer -> Bool
iscube n k 
 | ((n == 0) || (n == 1)) = True
 | (k^3 < n) = iscube n (k+1)
 | (k^3 == n) = True
 | (k^3 > n) = False
 | otherwise = False 

previousCube :: Integer -> Integer
previousCube n 
 | (iscube n 1 == True) = n
 | otherwise = previousCube (n-1)

-- Problem 3
digits :: Integer -> Integer
digits n 
 | ((n > 0) && (n < 10)) = 1
 | otherwise = 1 + digits(div n 10)
 
ispalin :: Integer -> Bool
ispalin n 
 | ((n >= 0) && (n < 10)) = True
 | (n `div` (10^(digits(n)-1))) == (mod n 10) = ispalin((n -(n `div` (10^(digits(n)-1)))*10^(digits(n)-1) - (mod n 10)) `div` 10)
 | otherwise = False

nextPalin :: Integer -> Integer
nextPalin n 
 | (n < 0) = 0               -- Alternatively (n < 0) = nextPalin (n+1)
 | ispalin n == True = n
 | otherwise = nextPalin (n+1)

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u 
 | ((l > u) || (u < 2)) = []
 | ((l == u) && (isPrime l == True))  = [l]
 | (isPrime l == True) = [l] ++ primesIn (l+1) u
 | (isPrime l == False) = primesIn (l+1) u

-- Problem 5
isprime :: Integer -> Integer -> Bool
isprime n k
 | (n < 2) = False
 | (n == 2) = True
 | ((k^2 <= n) && (mod n k == 0)) = False
 | (k^2 > n) = True
 | otherwise = isprime n (k+1)

isPrime :: Integer -> Bool
isPrime n = isprime n 2

-- Problem 6
dectobin :: Integer -> Integer -> Integer
dectobin n r
 | (n == 0) = 0
 | (n == 1) = 10^r
 | otherwise = (mod n 2)*(10^r) + dectobin (div n 2) (r+1) 

decToBin :: Integer -> Integer
decToBin n = dectobin n 0

-- Problem 7
bintodec :: Integer -> Integer -> Integer
bintodec n r  
 | (n == 0) = 0
 | (n == 1) = 2^r
 | otherwise = (mod n 10)*2^r + bintodec (div n 10) (r+1)

binToDec :: Integer -> Integer
binToDec n = bintodec n 0

-- Verification
verify = [[fst(z) == snd(z)| z <- (zip [f(x)|x<-inputs] outputs)] | (f, inputs, outputs) <- [(nextSquare, [-10,0,1,25,36,102], [0,0,1,25,36,121]),
 (previousCube, [1,100,1000,5000,10000], [1,64,1000,4913,9261]), (nextPalin, [-10,0,5,55,56,102,5962], [0,0,5,55,66,111,5995]), 
 (decToBin, [0, 1, 25, 63, 64, 65, 2^24], [0, 1, 11001, 111111, 1000000, 1000001, 1000000000000000000000000]), 
 (binToDec, [0,1,11001,111111,1000000,1000001,1001100011],[0,1,25,63,64,65,611])]]

--(isPrime, [-20, 0,1,2,5,27,47], [False, False, False, True, True, False, True]),