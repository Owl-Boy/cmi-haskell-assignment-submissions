-- Problem 1
nextSquare :: Integer -> Integer
nextSquare n
   | n <= 0    = 0
   | n > 0    = searchSquare n 0 where
   searchSquare r t
      | r > (t ^ 2)                      = searchSquare r (t + 1)
      | r <= (t ^ 2)                     = t ^ 2



-- Problem 2
previousCube :: Integer -> Integer
previousCube n
   | n <= 0    = error "please input only positive integers"
   | n > 0    = searchCube n 0 where
   searchCube r t
      | r >= (t ^ 3) && r < ((t + 1)^3)      = t ^ 3
      | r > (t ^ 3)                          = searchCube r (t + 1)



-- Problem 3
numLength :: Integer -> Integer
numLength n 
   | n < 0     = error "negative number invalid input"
   | n < 10    = 1
   | n > 10    = 1 + numLength (div n 10)

isPalin :: Integer -> Bool
isPalin n = n == revNum n
revNum :: Integer -> Integer
revNum n = if n < 10
         then n
         else revNum(div n 10) + ((mod n 10) * 10^(numLength n-1))

nextPalin :: Integer -> Integer
nextPalin n 
   | n <= 0  = 0
   | n > 0   = if isPalin n then n else nextPalin (n + 1)

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn m n
   | n < m           = []
   | n < 0 || m < 0  = []
   | otherwise       = if isPrime m then m : primesIn (m + 1) n else primesIn (m + 1) n

-- Problem 5

isPrime :: Integer -> Bool
isPrime n = if n < 2 then False else primeIt n 2 where
   primeIt n m
      | n == m          = True
      | n `mod` m /= 0  = primeIt n (m + 1)
      | n `mod` m == 0  = False

-- Problem 6
decToBin :: Integer -> Integer
decToBin n
   | n == 0        = 0
   | otherwise     = n `mod` 2 + 10 * decToBin (n `div` 2)


-- Problem 7
binToDec :: Integer -> Integer
binToDec n 
   | n == 0        = 0
   | otherwise     = n `mod` 10 + 2 * binToDec (n `div` 10)