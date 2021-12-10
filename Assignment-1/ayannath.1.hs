-- Submission by Ayan Nath

-- Problem 1
nextSquare :: Integer -> Integer
nextSquare n 
  | n<=0 = 0
  | otherwise = searchFrom 1 where
      searchFrom :: Integer -> Integer
      searchFrom x = if (x^2 >= n) && ((x-1)^2 < n) then x^2 else searchFrom (x+1)


-- Problem 2
previousCube :: Integer -> Integer
previousCube n 
  | n<=0 = error "Only positive integer inputs accepted."
  | otherwise = searchFrom 1 where
      searchFrom :: Integer -> Integer
      searchFrom x = if (x^3 <= n) && ((x+1)^3 > n) then x^3 else searchFrom (x+1)

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin n 
  | n<=0 = 0
  | otherwise = if isPalin n then n else nextPalin (n+1) where 
    numLen :: Integer -> Integer
    numLen n = if n < 10 then 1 else 1 + numLen (n `div` 10)

    revNum :: Integer -> Integer
    revNum n 
      | n < 10 = n 
      | otherwise =  revNum (n `div` 10) + (n `mod` 10) * 10^(numLen n - 1)

    isPalin :: Integer -> Bool
    isPalin n = n == revNum n

-- Problem 4
-- Here, I am using isPrime from Problem 5.
primesIn :: Integer -> Integer -> [Integer]
primesIn l u 
  | (u<l) || (u<=0) = []
  | l<0 = primesIn 0 u
  | otherwise = if isPrime l then l:primesIn (l+1) u else primesIn (l+1) u

-- Problem 5
isPrime :: Integer -> Bool
isPrime n
  | n<=1 = False
  | otherwise = searchFrom 2 where 
      searchFrom :: Integer -> Bool
      searchFrom d 
        | d>=n = True
        | otherwise = if (n `mod` d == 0) then False else searchFrom (d+1)

-- Problem 6
decToBin :: Integer -> Integer
decToBin n 
  | n<0 = error "Only non-negative integer inputs accepted."
  | n==0 = 0
  | otherwise = 10*(decToBin p) + q where 
      (p,q) = divMod n 2

-- Problem 7
binToDec :: Integer -> Integer
binToDec n
  | n<0 = error "Only non-negative integer inputs accepted."
  | n==0 = 0
  | otherwise = 2*(binToDec p) + q where 
      (p,q) = divMod n 10
