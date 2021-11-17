nxtsqr :: Integer -> Integer -> Integer
nxtsqr a b
    | a <= 0 = 0
    | (b-1)^2 < a && a <=b^2 = b^2
    | b^2 < a = nxtsqr a (b+ 1)

-- Problem 1
nextSquare :: Integer -> Integer
nextSquare x = nxtsqr x 1



prccbe :: Integer -> Integer -> Integer
prccbe a b
    | b^3 <= a && a <(b+1)^3 = b^3
    | (b+1)^3 <= a = prccbe a (b+1)
    | otherwise = prccbe a (b-1)

-- Problem 2
previousCube :: Integer -> Integer
previousCube x = prccbe x 0




inttolst:: Integer -> [Integer]
inttolst a
    |a<=0 = [0]
    |a < 10 = [a]
    |otherwise = inttolst(div a 10) ++ [mod a 10]

lsttoint:: [Integer] -> Integer
lsttoint [a] = a
lsttoint (x:xs) = x*(10^length xs) + lsttoint xs

incrsepalin::Integer -> Integer -> Integer
incrsepalin a b
    |a>= b     =  a
    |even len  =  lsttoint( reverse (incrselst lst2) ++ incrselst lst2 )
    |otherwise =  lsttoint( reverse (tail (incrselst lst2 )) ++ incrselst lst2 )
        where   lst  = inttolst a
                len  = length lst
                d    = len `div` 2
                lst2 = drop d lst

incrselst :: [Integer] -> [Integer]
incrselst [] = []
incrselst (x:xs) =if x /= 9 then (x+1):xs else 0:incrselst xs

-- Problem 3
nextPalin :: Integer -> Integer
nextPalin a
    |even len  =  incrsepalin pae a
    |otherwise =  incrsepalin pao a
        where lst = inttolst a
              len = length lst
              d   = len `div` 2
              pae = lsttoint (take d lst ++ reverse (take d lst))
              pao = lsttoint (take (d+1) lst ++ reverse (take d lst))






primesieve:: Integer -> [Integer] -> [Integer]
primesieve x [] = [x]
primesieve x (y:ys)
    | mod x y == 0 = y:ys
    |otherwise     = y:primesieve x ys

primelist :: Integer -> [Integer]
primelist x
    |x <=1 =[]
    |otherwise = primesieve x (primelist (x-1))

-- Problem 4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u = drop (length (primelist(l-1))) (primelist u)

-- Problem 5
isPrime ::Integer -> Bool 
isPrime x = x == last (primelist x)






-- Problem 6
decToBin :: Integer -> Integer
decToBin 0 = 0
decToBin a = mod a 2 + 10*decToBin(div a 2)




-- Problem 7
binToDec :: Integer -> Integer
binToDec 0 = 0
binToDec a = mod a 10 + 2*binToDec(div a 10)