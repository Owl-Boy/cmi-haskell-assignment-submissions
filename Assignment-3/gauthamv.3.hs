import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )

-- Question 1
selectLeader :: Int -> Int -> Int 
selectLeader n k = selectLeaderHelp [1..n] k 0

selectLeaderHelp :: [Int] -> Int -> Int -> Int
selectLeaderHelp [c] _ _ = c
selectLeaderHelp cs k h = selectLeaderHelp modifiedcs k next where
                          modifiedcs = cs `delete` h
                          next = (h + k - 1) `mod` (length modifiedcs)

delete :: [Int] -> Int -> [Int]
delete [] _ = error "delete - index out of bounds"
delete (x:xs) 0 = xs
delete (x:xs) n = x : (delete xs (n-1))

-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = selectLeaderHelp' [1..n] k 0

selectLeaderHelp' :: [Int] -> Int -> Int -> [[Int]]
selectLeaderHelp' [c] _ _ = [[c]]
selectLeaderHelp' cs k h = cs : (selectLeaderHelp' modifiedcs k next) where
                           modifiedcs = cs `delete` h
                           next = (h + k - 1) `mod` (length modifiedcs)

-- Question 3
cf :: Rational -> [Integer]
cf r
 | q == 1 = [p-1, 1]
 | otherwise = d:(cf $ reciprocal (m%q)) 
 where
     (d, m) = p `divMod` q
     (p, q) = (numerator r, denominator r)

computeRat :: [Integer] -> Rational
computeRat [] = error "Empty list given to computeRat"
computeRat [n] = (n%1)
computeRat (n:ns) = (n%1) + (reciprocal $ computeRat ns)

reciprocal :: Rational -> Rational
reciprocal 0 = error "NaN in reciprocal"
reciprocal n = (denominator n) % (numerator n)

-- Question 4
root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

errorval :: Rational -> Double
errorval x = abs (root6 - evalRat x)

rt6cf :: [Integer]
rt6cf = 2:(cycle [2, 4])

approxRoot6 :: Double -> Rational 
approxRoot6 epsilon = approxRoot6Help 1 epsilon

approxRoot6Help n epsilon = if errorval approx < epsilon then approx else approxRoot6Help (n+1) epsilon where
                            approx = computeRat $ take n rt6cf

-- Question 5 
mss :: [Int] -> Int 
mss xs = if allNegative prepped then sum prepped else
         maximum (((mssmain . (0:)) prepped) : (prepped)) where
         prepped = prep xs

allNegative :: [Int] -> Bool
allNegative [] = True
allNegative (x:xs) = if x >= 0 then False else allNegative xs

prep :: [Int] -> [Int]
prep = (msscompress . map(^3))

mssmain :: [Int] -> Int
mssmain [] = 0
mssmain [x] = if x >= 0 then x else 0
mssmain (x:y:xs)
 | x < 0 = mssmain (y:xs)
 | (x+y) >= 0 = mssmain ((x+y):xs)
 | otherwise = mssmain (0:xs)

msscompress :: [Int] -> [Int]
msscompress [] = []
msscompress [x] = [x]
msscompress (x:y:xs) = if (x*y) >= 0 then msscompress ((x+y):xs)
                                     else x:(msscompress (y:xs))


-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps as = last (last (lpsTab as))

lpsTab :: Eq a => [a] -> [[(Int, [a])]]
lpsTab as = fstRow:(zipWith g (lpsTab as) [1..n]) where
           n = length as
           fstRow = replicate (n+1) (0, [])
           g l m = [f m j l as | j <- [0..n]]
           
f :: Eq a => Int -> Int -> [(Int, [a])] -> [a] -> (Int, [a])
f i j l as = if safetyFlag then (0, []) else
             if (i+j) == (n+1) then (1, [x]) else
             if ind > 0 then (maxfst, maxsnd) else 
             if ind == 0 then max2 else (u1, u2) where
    safetyFlag = ((i+j) <= n)
    n = length as
    x = as!!(n-i)
    ind = indexB (x) (take (i+j-n-1) (drop (n-i+1) as))
    (u1, u2) = (l!!j)
    (t1, t2) = (l!!(n+1+ind-i))
    (maxfst, maxsnd) = if u1 >= (t1+2) then (u1, u2) else (t1+2, (x:t2) ++ [x])
    max2 = if 2 >= u1 then (2, replicate 2 x) else (u1, u2)

indexB :: Eq a => a -> [a] -> Int
indexB _ [] = -1
indexB c s = if c == l then (length s - 1) else indexB c i where
             l = last s
             i = init s