--Problem 1     
nextSquare :: Integer -> Integer
nextSquare n
 | n<=0 = 0
 |otherwise = m^2  where m = ceiling.sqrt.fromIntegral $n
-- Problem 2
previousCube :: Integer -> Integer
previousCube n 
 |(round.(**(1/3)).fromIntegral $n)^3 == n =n
 |otherwise = m^3  where m = floor.(**(1/3)).fromIntegral $n
-- Problem 3
nextPalin :: Integer -> Integer
nextPalin n
 |n<=0 = 0 
 | rev(0,n) == n = n
 |otherwise = nextPalin(n+1)
 where rev (a,b)
        |b == 0 =a 
        |otherwise = rev(10*a+q,p) where (p,q)= divMod b 10
-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u 
 |u<=1 = []
 |l>u = []
 |isPrime l = l : primesIn (l+1) u
 |otherwise = primesIn (l+1) u
-- Problem 5
isPrime :: Integer -> Bool
isPrime n 
 |n<=1 = False 
 |otherwise = isdiv (floor.sqrt.fromIntegral $n) n 
 where isdiv a b 
         |a==1 = True
         |b `mod` a == 0 = False
         |otherwise = isdiv (a-1) b
-- Problem 6
decToBin :: Integer -> Integer
decToBin n 
 |n == 0 = 0
 |otherwise =10*decToBin p+q where (p,q)= divMod n 2
-- Problem 7
binToDec :: Integer -> Integer
binToDec n 
 |n == 0 = 0
 |otherwise = q + 2*binToDec(p) where (p,q)= divMod n 10
