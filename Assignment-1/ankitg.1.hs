--NAME:Ankit Gayen , ROLL NO. : BMC202116
import Data.List
-- problem 1
nextSquare :: Integer -> Integer 
nextSquare n = if n <=0
    then 0
    else (ceiling ( sqrt ( fromIntegral n)))^2

-- problem 2
previousCube :: Integer -> Integer
previousCube n = (floor( fromIntegral n**(1/3)))^3

-- problem 3
nextPalin :: Integer -> Integer
nextPalin n = if show n  == reverse ( show n)
    then n
    else nextPalin (n+1)

-- problem 4
primeCheck :: Integer -> Integer -> Bool
primeCheck 1 k = True
primeCheck n k = if k<=0 || n<=0 then False
    else if  ( k `mod` n ==0) && n>1 
    then False 
    else primeCheck (n-1) k
primesIn :: Integer -> Integer -> [Integer]
primesIn l u = if (primeCheck ( l `div` 2) l) && l<=u
    then l:primesIn (l+1) u
    else if l>u then []
    else []++primesIn (l+1) u

-- problem 5
isPrime :: Integer -> Bool
isPrime n = if primeCheck (n `div`2) n
    then True 
    else False

-- problem 6
bin :: Integer -> [Integer]
bin 0 = []
bin n = if (n `mod` 2)== 1
     then 1:bin (n `div` 2 )
     else 0:bin (n `div` 2 )
bin2  :: Integer -> [Integer]
bin2 n = reverse(bin n)
decToBin :: Integer -> Integer
decToBin n = if n==0
    then 0
    else read( concat (map show (bin2 n))) :: Integer

-- problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec n = 2*binToDec (div n 10)+(mod n 10)
