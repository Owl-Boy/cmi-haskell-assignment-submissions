import Data.Array
import Data.Ratio

--1
selectLeader :: Int -> Int -> Int
selectLeader n k = head(sl n k ([1..n], 0))

sl :: Int -> Int -> ([Int], Int) -> [Int]
sl 1 k l = fst(l)
sl n k l = sl (n - 1) k ([m | m <- fst(l), m /= fst(l) !! (snd(l) `mod` n)], (snd(l) + k - 1) `mod` (n - 1)) 

selectLeader_tests = [
    selectLeader 100 5 == 43,
    selectLeader 5 2 == 2,
    selectLeader 10 5 == 9]

--2
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = sl' n k ([1..n], 0)

sl' :: Int -> Int -> ([Int], Int) -> [[Int]]
sl' 1 k l = [fst(l)]
sl' n k l = [fst(l)] ++ sl' (n - 1) k ([m | m <- fst(l), m /= fst(l) !! (snd(l) `mod` n)], (snd(l) + k - 1) `mod` (n - 1))

selectLeader'_tests = [
    selectLeader' 10 5 == [[1,2,3,4,5,6,7,8,9,10],
    [ 2,3,4,5,6,7,8,9,10],
    [ 2,3,4,5, 7,8,9,10],
    [ 3,4,5, 7,8,9,10],
    [ 3,4,5, 7, 9,10],
    [ 3,4, 7, 9,10],
    [ 3, 7, 9,10],
    [ 3, 9,10],
    [ 3, 9 ],
    [ 9 ]],
    selectLeader' 9 4 == [[1,2,3,4,5,6,7,8,9],
    [ 2,3,4,5,6,7,8,9],
    [ 2,3,4, 6,7,8,9],
    [ 2,3,4, 6,7,8 ],
    [ 2,3,4, 7,8 ],
    [ 2, 4, 7,8 ],
    [ 4, 7,8 ],
    [ 7,8 ],
    [ 7 ]]]

--3
computeRat :: [Integer] -> Rational
computeRat (l:ls) = if ls /= [] then l%1 + 1/computeRat(ls) else l%1

cf :: Rational -> [Integer]
cf x = [floor(x)] ++ if numerator(x - fromIntegral(floor(x))%1) == 1 then [fromIntegral(denominator(x))] else cf (1/(x - fromIntegral(floor(x))%1))

computeRat_cf_tests = [
    cf (33%42) == [0,1,3,1,2],
    cf (22%7) == [3,7],
    cf (23%7) == [3,3,2],
    cf (26%21) == [1,4,5],
    cf (-26%21) == [-2,1,3,5],
    cf (-42%31) == [-2,1,1,1,4,2],
    computeRat [3,3,2] == 23%7,
    computeRat [3,7] == 22%7,
    computeRat [1,4,5] == 26%21,
    computeRat [-1,1,1,1,1] == (-2)%5 ]

--4
root6 :: Double
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approx :: Double -> Double -> Double -> [Integer] -> Rational 
approx x e y l = if abs(x - evalRat(computeRat(l))) < e then computeRat(l) else approx x e (1 / (y - fromIntegral(floor(y)))) (l ++ [fromIntegral(floor y)]) 

approxRoot6 :: Double -> Rational                              
approxRoot6 e = approx root6 e (1 / (root6 - 2)) [2]

approxRoot6_tests = [
    approxRoot6 0.0001 == 218 % 89, 
    approxRoot6 0.000001 == 2158 % 881, 
    abs(approxRoot6 0.0000000000001 -  20721118 % 8459361) < 2*0.0000000000001]

--5
third :: (Int, Int, Int) -> Int
third (a, b, c) = c
 
second :: (Int, Int, Int) -> Int
second (a, b, c) = b

first :: (Int, Int, Int) -> Int
first (a, b, c) = a 

mss :: [Int] -> Int
mss l = third (msum l' (length l - 1)) where l' = listArray (0, length l - 1) l

msum :: Array Int Int -> Int -> (Int, Int, Int)              
msum l 0 = ((l ! 0)^3, 0, (l ! 0)^3)
msum l n = if first(prev) > 0 then (if (first(prev) + (l ! n)^3 > third(prev)) then (first(prev) + (l ! n)^3, second(prev), first(prev) + (l ! n)^3) else (first(prev) + (l ! n)^3, second(prev), third(prev))) else (if ((l ! n)^3 > third(prev)) then ((l ! n)^3, n, (l ! n)^3) else ((l ! n)^3, n, third(prev))) where prev = msum l (n-1)

mss_tests = [
    mss [4,10,3,12,-1,1,-1,-7 ,6,-13] == 2819,
    mss [4,6,1,4,0,4,5,-3,2,8,-5,2,2,-1,-2,0,9,2,10,2] == 2654,
    mss [-6,11,7,18,-13,-13,11,11,4,8,-12,-4,-3,-15,20] == 9156,
    mss [4,4,-4,4,-5,-4,-4,-4,-7,8,-4,2,2,-7,3,5,6,-1,2,-3] == 512]

--6
lps :: (Eq a, Ord a) => [a] -> (Int, [a])
lps [] = (0, [])
lps l = (lpsArr ! (0, n)) where
    n = length(l)
    lArr = listArray (0, n - 1) l
    lpsArr = listArray ((0, 0), (n, n)) [lp i j | i <- [0..n], j <- [0..n]]
    lp i j 
        | i == j = (0, [])
        | i + 1 == j = (1, [lArr ! i])
        | otherwise = if lArr ! i == lArr ! (j - 1) then (fst(lpsArr ! (i + 1, j - 1)) + 2, [(lArr ! i)] ++ snd(lpsArr ! (i + 1, j - 1)) ++ [(lArr ! (j - 1))]) else max (lpsArr ! (i, j - 1)) (lpsArr ! (i + 1, j))

lps_tests = [
    fst(lps "gultnhebrpuuowjrtpfw") == fst(6,"truurt"),
    fst(lps "edgnqqavflgfpzubuiaq") == fst(7,"qaflfaq"),
    fst(lps "nndrvtgiorptiateaihs") == fst(5,"tioit"),
    fst(lps "quwzpirtwnrazkbjrzcc") == fst(5,"zrtrz")]

verify = False `notElem` [False `notElem` x | x <- [selectLeader_tests, selectLeader'_tests, computeRat_cf_tests, approxRoot6_tests, mss_tests, lps_tests]]

--5 (Shubh Sharma)
mssShubh :: [Int] -> Int 
mssShubh (x:xs) = helper (x ^ 3) (x ^ 3) xs
  where
    helper finalVal candidateVal ls
      | finalVal < candidateVal = helper candidateVal candidateVal ls
      | null ls = finalVal
      | candidateVal <= 0 = helper finalVal (head ls ^ 3) (tail ls)
      | otherwise = helper finalVal (candidateVal + head ls ^ 3) (tail ls) 