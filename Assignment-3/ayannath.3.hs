import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!), bounds )

-- Submitted by Ayan Nath

-- Question 1
selectLeader :: Int -> Int -> Int 
selectLeader 1 k = 1
selectLeader n k = selectLeaderHelper (tail l, 0) k where
    l = [1..n]

    selectLeaderHelper (xs, prevDel) k = if length xs == 1
                                            then head xs
                                            else selectLeaderHelper l' k where 
                                                  l' = deleteKid (xs, prevDel) k

deleteKid (xs, prevDelInd) k = (xs', newInd) where 
    newInd = (prevDelInd + k - 1) `mod` length xs
    xs' = deleteAt newInd xs

deleteAt i xs = left ++ right where
    (left, (_:right)) = splitAt i xs

-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' 1 k = [[1]]
selectLeader' n k = l:(map fst efficientList) where
    l = [1..n]
    efficientList = ([2..n],0):[deleteKid (efficientList !! i) k | i <- [0..(n-3)]]

-- Question 3
cf :: Rational -> [Integer]
cf rat  
    | m == 0 = [d]
    | m == 1 = [d,q] 
    | otherwise = d:cf (q % m) where 
        p = numerator rat
        q = denominator rat
        (d,m) = divMod p q

computeRat :: [Integer] -> Rational
computeRat ns = computeRat' rats where 
    rats = reverse [fromIntegral n :: Rational | n <- ns]
    computeRat' rs 
      | length rs == 1 = head rs 
      | otherwise = computeRat' $ resolve rs
    resolve (a:b:rs) = (recip a + b):rs

-- Question 4
root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

cfRoot6 :: [Integer]
cfRoot6 = 2:[(\x -> if even x then 2 else 4) i | i <- [0..]]

approxRoot6 :: Double -> Rational 
approxRoot6 epsilon = if epsilon <= 0 then error "Bad request: non-positive argument" else result where 
    result = computeRat $ take (searchFrom 1) cfRoot6
    searchFrom i 
      | abs (root6 - evalRat (computeRat $ take i cfRoot6)) < epsilon = i 
      | otherwise = searchFrom (i+1) 

-- Question 5 
-- Time complexity: O(n) 
mss :: [Int] -> Int 
mss [] = 0
mss ys = maxDiff where 
    xs = 0:ys
    n = length xs
    cxsArr = listArray (0, (n-1)) (map (^3) xs) 
    cummxsArr = listArray (0, (n-1)) ((cxsArr ! 0):[(cummxsArr ! i) + (cxsArr ! (i+1))| i <- [0..(n - 2)]])
    maxArr = listArray (0, (n-1)) ((cummxsArr ! (n-1)):[ max (maxArr ! i) (cummxsArr ! (n-2-i)) | i <- [0..(n-2)] ])
    maxDiff = maximum [ (maxArr ! i) - (cummxsArr ! (n-2-i)) | i <- [0..(n-2)]]

-- Question 6
-- Paradigm: Dynamic Programming
-- Time complexity: O(n^2) ?
lps :: Eq a => [a] -> (Int, [a])
lps xs = result where
    result = f 0 n
--    result = dpMatrix ! (n,0)
    n = (length xs) - 1
    xsArr = listArray (0, n) xs
    subArr i j = listArray (i,j) (map (xsArr !) [i..j])
    dpMatrix = listArray ((0,0), (n,n)) [ f i j | i <- (reverse [0..n]), j <- (reverse [0..n]) ]

    f i j 
      | (i > j) || (j > n) = (0, [])
      | i == j = (1, [xsArr ! i])
      | headLastOccurIx == i = fValue1
      | otherwise =  if (fst otherwiseValue) > (fst fValue1)
                        then otherwiseValue
                        else fValue1 where 
              headLastOccurIx = headLastOccur $ subArr i j
              x1 = xsArr ! i
              -- fValue1 = f (i+1) j
              fValue1 = dpMatrix ! (n-i-1,n-j)
              -- fValue2 = f (i+1) (headLastOccurIx - 1)
              fValue2 = dpMatrix ! (n-i-1, n-headLastOccurIx + 1)
              otherwiseValue = (2 + fst fValue2, (x1:(snd fValue2)) ++ [x1])

    headLastOccur arr = searchfrom j where 
        (i,j) = bounds arr
        searchfrom k = if (arr ! k) == x1 then k else searchfrom (k-1) where 
            x1 = arr ! i
