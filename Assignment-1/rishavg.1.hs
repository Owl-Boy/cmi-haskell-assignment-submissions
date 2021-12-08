nextSquare :: Integer -> Integer
nextSquare a
 |a<=0 =0
 |otherwise = ns 1 a
 where
     ns :: Integer -> Integer -> Integer
     ns c b = if c*c>=b then c*c else ns (c+1) b

previousCube :: Integer -> Integer
previousCube a
 |a<0 = -1*(nc 1 (-1*a))
 |otherwise = pc 0 a
 where
     nc :: Integer -> Integer -> Integer
     nc c b = if c*c*c>=b then c*c*c else nc (c+1) b 
     pc :: Integer -> Integer -> Integer
     pc c b = if b <(c+1)^3 then c*c*c else pc (c+1) b

nextPalin :: Integer -> Integer
nextPalin a
 |a<=0 = 0
 |ispal a = a
 |otherwise = nextPalin(a+1)
 where
     ispal :: Integer -> Bool
     ispal x = if rev 0 x == x then True else False
     rev :: Integer -> Integer -> Integer
     rev  b c = if c==0 then b else rev (10*b + (c `mod` 10)) (c `div` 10)

primesIn :: Integer -> Integer -> [Integer]
primesIn a b
 |a>b = []
 |b<=0 =[]
 |a<0 = primesIn 0 b
 |otherwise = if isPrime a then a:(primesIn (a+1) b) else (primesIn (a+1) b)
isPrime :: Integer -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime a = if divs 1 a == [1,a] then True else False
 where
     divs :: Integer->Integer -> [Integer]
     divs x y = if x==y then [x] else (if y `mod` x ==0 then x:(divs (x+1) y) else divs (x+1) y) 

decToBin :: Integer -> Integer
decToBin a = basec 2 a
 where
     baseaux :: Integer -> Integer -> [Integer]
     baseaux b c = if b>c  then [c] else (baseaux b (c `div` b))++[c `mod` b]
     basec :: Integer -> Integer -> Integer
     basec b c = convert (baseaux b c) ((mlength (baseaux b c)) -1) 
     convert :: [Integer] -> Integer -> Integer
     convert [] i = 0
     convert (x:xs) i = (10^i)*x + (convert (xs) (i-1))
     mlength :: [a] -> Integer
     mlength [] = 0
     mlength (x:xs) = 1+(mlength xs)

binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec 1 = 1
binToDec x =( x `mod` 10) + 2*(binToDec (x `div` 10))