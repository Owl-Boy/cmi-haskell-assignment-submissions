
--1
sqr :: Integer -> Integer 
sqr x = x*x

isSquare :: Integer -> Bool
isSquare n = if n == sqr(ceiling(sqrt(fromIntegral n :: Double))) then True else False

nextSquare :: Integer -> Integer 
nextSquare n = if isSquare n then n else nextSquare (n +1)


--2 
previousCube :: Integer -> Integer 
previousCube n = last [x^3 | x <- [0..n], x^3 - n <= 0]

--3
numLen :: Integer -> Integer 
numLen n
     | n <= 0 = 0
     | n <= 9 = 1
     | otherwise = numLen (div n 10) + 1

revNum :: Integer -> Integer 
revNum 0 = 0
revNum n = (mod n 10)*10^(numLen n -1) + revNum (div n 10)

nextPalin :: Integer -> Integer 
nextPalin n
      | n <= 0 = 0
      | otherwise = if n == revNum n then n else nextPalin (n + 1)



--4 
primesIn :: Integer -> Integer -> [Integer]
primesIn n m = [x | x <- [n..m ] , isPrime x == True]

--5 
factor :: Integer -> [Integer]
factor n = [x | x <- [2..(n-1)], mod n x == 0]
 
isPrime :: Integer -> Bool
isPrime n 
      | n <= 1 = False
      | otherwise = null(factor n)

--6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin n = mod n 2 +10*(decToBin (div n 2))

--7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec n = mod n 10 + 2*(binToDec(div n 10))

apply :: Integer -> Integer -> Integer -> [Integer]
apply l m n = 
    
        | l == m = error "its too hard"
        | l > m && m == n = [l,m]
        | l > m && m < n = if l >= n then [l] else []
        | l > m && m > n = l : apply m (2*m - l) n
        | l < m && m == n = [l,m]
        | l < m && m < n = l : apply m (2*m - l) n
        | l < m && m > n = if l >= n then [l] else []





        








