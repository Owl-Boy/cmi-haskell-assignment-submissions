nextSquare :: Integer -> Integer 
nextSquare n = searchfrom 0 where
    searchfrom x 
             | x*x >= n = x*x
             | otherwise = searchfrom (x+1)

previousCube :: Integer -> Integer 
prevcubes :: Integer -> [Integer]
prevcubes 1 = [1]
prevcubes x = choose x where
    choose y
        | y^3 > x = choose (y-1)
        | y^3 <=x = y:choose (y-1)
previousCube n = head (prevcubes n) ^3            

ispalin :: Integer -> Bool 
revnum :: Integer -> Integer 
numlength :: Integer -> Integer
nextPalin :: Integer -> Integer 
numlength n = if n<10 then 1 else 1 + numlength (n `div` 10)
revnum x = if x<10 then x else (x `mod` 10)*10^(numlength x - 1)+revnum(x `div` 10)
ispalin y = y == revnum y 
nextPalin x 
        | x < 0 = 0
        | ispalin x = x
        | otherwise = nextPalin(x+1)

numdiv :: Integer -> Integer
primesIn :: Integer -> Integer -> [Integer]
numdiv 1 = 1
numdiv n = searchfrom n where
    searchfrom x
            | x == 0 = 0
            | n `mod` x==0 = 1+searchfrom (x-1)
            | n `mod` x/=0 = searchfrom (x-1)
           
primesIn l u 
        | l>u = []
        | l<0 && u<=0 = []
        | l<0 && u>0 = primesIn 1 u
        | l==0 && u>0 = primesIn 1 u
        | l>0 && l==u && numdiv u ==2 = [u]
        | l>0 && l==u && numdiv u /=2 = [] 
        | l>0 && l<u && numdiv (head [l..u]) == 2 = (head [l..u]) : primesIn (l+1) u
        | l>0 && l<u && numdiv (head [1..u]) /= 2 = primesIn (l+1) u

isPrime :: Integer -> Bool
isPrime n = n>0 && numdiv n == 2

decToBin :: Integer -> Integer 
decToBin 0 = 0
decToBin 1 = 1
decToBin n = (n `mod` 2) + decToBin (n `div` 2) *10

binToDec :: Integer -> Integer 
binToDec 0 = 0
binToDec 1 = 1
binToDec n = (n `mod` 10) + binToDec (n `div` 10) *2






