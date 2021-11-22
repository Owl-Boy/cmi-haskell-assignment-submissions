numberOfDiv n 1=2
numberOfDiv n m 
    |n `mod` m==0  =1 + numberOfDiv n (m-1)
    |otherwise =numberOfDiv n (m-1)

--1

nextSquare :: Integer->Integer
nextSquare n
    |n<=0 =0
    |(numberOfDiv n n-1) `mod` 2==1 =n
    |otherwise = nextSquare(n+1)
--2

previousCube :: Integer -> Integer
previousCube n=floor(realToFrac n **(1/3))^3
--3

numLen :: (Num p, Integral t) => t -> p
numLen n= if n<10 then 1 else 1 + numLen (div n 10)
revNum n= if n<10 then n else (n `mod` 10)*10^(numLen n-1) + revNum (n `div` 10) 
nextPalin :: Integer -> Integer
nextPalin n
    |n<=0 = 0
    |n==revNum n  =n
    |otherwise =nextPalin (n+1)
--4

primesIn :: Integer-> Integer-> [Integer]
primesIn n m
    |m<n = []
    |m<=0 = []
    |n<0 = primesIn 0 m
    |numberOfDiv m m-1==2 = primesIn n (m-1) ++ [m]
    |otherwise = primesIn n (m-1)
--5

isPrime ::Integer->Bool
isPrime n   
    |n<=1  = False
    |numberOfDiv n n-1 ==2 = True
    |otherwise = False 
--6

decToBin :: Integer -> Integer
decToBin n= if n<2 then n else (n`mod` 2)+10*decToBin(n `div` 2)
--7

binToDec :: Integer -> Integer
binToDec n=if n==1 then 1 else (n `mod` 10)+2*binToDec(n `div` 10)