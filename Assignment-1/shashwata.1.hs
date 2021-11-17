--Probelem 1
nextSquare :: Integer -> Integer
nextSquare x = localFunction [x,1] where
	localFunction :: [Integer] -> Integer
	localFunction [x,y]
                    | x <= 0 = 0
                    | x==y^2 = y^2 
	            | div x (y^2) == 0  = y^2
                    | otherwise = localFunction [x,y+1]

--Problem 2
previousCube :: Integer -> Integer
previousCube x = localFunction [x,1] where
	localFunction  :: [Integer] -> Integer
	localFunction [x,y]
	            | (y^3<=x)&&((y+1)^3 > x) = y^3
	            | otherwise = localFunction [x,y+1]

--Problem 3
nextPalin :: Integer -> Integer
nextPalin x = if x<0 then 0 else if revNum x == x then x else nextPalin (x+1) where
     revNum :: Integer -> Integer
     revNum x = localFunction [x,0] where
		localFunction :: [Integer] -> Integer
		localFunction [x,y]
			    | x==0 = y
			    | otherwise = localFunction [div x 10 , 10*y+(mod x 10)]

 

--Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn x y
       |x>y  = []
       |x==y && (not (f x)) = []
       |x==y && f x = [x]
       |f x = [x] ++ primesIn (x+1) y 
       |otherwise  = primesIn (x+1) y where
	f :: Integer -> Bool
	f x = if localFunction [1,x] == 1 then True else False where
          localFunction :: [Integer] -> Integer
	  localFunction [z,y]
	              |y< 1 = 0
	       	      |z==y = 0 
                      |mod y z == 0 = 1 + localFunction [z+1,y]
                      |otherwise = localFunction [z+1 , y]
  

--Problem 5
isPrime :: Integer -> Bool
isPrime x = if localFunction [1,x]==1 then True else False where
	localFunction :: [Integer] -> Integer
	localFunction [z,y]
	            |y< 1 = 0
		    |z==y = 0 
                    |mod y z == 0 = 1 + localFunction [z+1,y]
                    |otherwise = localFunction [z+1 , y]
--Problem 6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin x = mod x 2 + 10*(decToBin (div x 2))


--Problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec x = mod x 10 + 2*(binToDec (div x 10))
