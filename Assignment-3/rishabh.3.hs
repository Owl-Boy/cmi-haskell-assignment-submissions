import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )

-- Question 1
selectLeader :: Int -> Int -> Int 
selectLeader n k = head $ last $ selectLeader' n k  

-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' 0 _ = [[]]
selectLeader' _ 0 = [[]]
selectLeader' n k = sl n 0 [1..n] where 
    sl 1 _ l = [l]
    sl a c l = l:sl (a-1) (mod (c+k-1) (a-1))  p where 
    -- sl a c l = if (c+k) <= (a-1) then l:sl (a-1) (c+k-1) p else l:sl (a-1) (mod (c+k-1) (a-1)) p where 
        p = remove l c where
            remove (l:ls) c = if c==0 then ls else l:remove ls (c-1) 

-- Question 3
cf :: Rational -> [Integer]
cf t
    |b == 1 = [a]
    |otherwise = q : cf (b%r) where 
        a = numerator t
        b = denominator t
        (q,r) = divMod a b
    

computeRat :: [Integer] -> Rational
computeRat [x] = x%1
computeRat (x:xs) = (x*a + b) % a where
    t = computeRat xs
    a = numerator t
    b = denominator t 

-- Question 4
root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational 
approxRoot6 e = try 2 where
    l = 2: cycle [2,4] 
    try n 
        | abs (evalRat r - root6) < e = r 
        | otherwise = try (n+1) where
            r = computeRat (take n l)


-- Question 5 
mss :: [Int] -> Int 
mss [] = 0
mss l = maximum $ newList $ map (^3) l

newList []  = []
newList [x]  = [x]
newList (x:xs)  =  max x (x + p) : q where
    q = newList xs
    p = head q 


-- reduce :: [Int] -> Int
-- reduce [] = []
-- reduce [x] = [x]
-- reduce (x:y:ys) = if x*y >= 0 then (x+y) : reduce ys else x : reduce (y:ys)    

-- mss [] = 0
-- mss ls = mssDP!(0,n-1) where
--     n = length ls 
--     mssDP = listArray ((0,0),(n-1,n-1)) [f i j | i <- [0..(n-1)], j<- [0..(n-1)]]
--     lsArr = listArray (0,n-1) (map (^3) ls) 
--     f i j 
--         | i > j = 0
--         | i == j = lsArr!i
--         | otherwise = max (mssDP!(i+1,j)) (mssDP!(i+1,j) + (lsArr!i))


-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps [] = (0,[])
lps ls = lpsDP ! (0,n-1) where
    n = length ls 
    lpsDP = listArray ((0,0),(n-1,n-1)) [f i j | i <- [0..(n-1)], j<- [0..(n-1)]]
    lsArr = listArray (0,n-1) ls 
    f i j 
        | i > j = (0,[])
        | i == j = (1,[lsArr!i])
        | lsArr ! i == lsArr ! j = (2 + fst (lpsDP!(i+1,j-1)), (lsArr!i : snd (lpsDP!(i+1,j-1))) ++ [lsArr!j]) 
        | otherwise = if len1 > len2 then (len1, seq1) else (len2,seq2) where
            (len1,seq1) = lpsDP!(i+1,j)
            (len2,seq2) = lpsDP!(i,j-1)

