-- Problem 1

isSquare :: Integer -> Bool
isSquare n = sq * sq == n
    where sq = ceiling $ sqrt $ (fromIntegral n::Double)

nextSquare :: Integer -> Integer
nextSquare n
 | n < 0 = 0 
 | isSquare n == True = n
 | isSquare n == False = nextSquare (n+1)

-- Problem 2

isCube :: Integer -> Bool
isCube n = cu ^ 3 == n
 where cu = ceiling ( ( fromIntegral n :: Double) ** (1/3))

previousCube :: Integer -> Integer
previousCube n
  | isCube n == True = n
  | otherwise        = previousCube (n-1)


-- Problem 3

nl :: Integer -> Integer
nl n 
  | n < 10     = 1
  | otherwise  = 1 + nl (div n 10)
 
revNum :: Integer -> Integer
revNum a
 | a < 10      = a
 | otherwise   = (mod a 10)* 10 ^ ((nl a)-1)  + revNum ( div a 10 )

isPalin :: Integer -> Bool
isPalin a = a == revNum a

nextPalin :: Integer -> Integer
nextPalin n 
  | n < 0             = 0
  | isPalin n == True = n
  | otherwise         = nextPalin (n+1)



-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn a b
  | a <= 0 && b <= 0 = []
  | a > b = []
  | b >= a = [n | n <- [a..b] , isPrime n == True ]


-- Problem 5

isPrime :: Integer -> Bool
isPrime n = if n > 1 then null[x | x <-[2..(n-1)], mod n x == 0 ] 
else False 

-- Problem 6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin n = mod n 2 + 10 * (decToBin ( div n 2 ))

-- Problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec n = mod n 10 + 2 * ( binToDec $ div n 10 )
