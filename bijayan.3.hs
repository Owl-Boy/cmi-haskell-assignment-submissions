{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import Data.Ratio ( (%), denominator, numerator, approxRational )
import Data.Array ( elems, listArray, array, (!), Array )

-- Question 1

selectLeader :: Int -> Int -> Int
selectLeader n k = removeFrom [1..n] 1 where
  removeFrom [a] _ = a
  removeFrom xs t = removeFrom (take (t-1) xs ++ drop t xs) (((t+k-2) `mod` (length xs-1))+1)

-- Question 2 

selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = removeFrom [1..n] 1 where
  removeFrom [a] _ = [[a]]
  removeFrom xs t = xs : removeFrom (take (t-1) xs ++ drop t xs) (((t+k-2) `mod` (length xs -1))+1 )

-- Question 3

cf :: Rational -> [Integer]
cf a = if denominator a == 1 then [fromIntegral(numerator a)]
else floor (fromIntegral (numerator a)/fromIntegral(denominator a)) : cf (denominator a % (numerator a - floor (fromIntegral (numerator a)/fromIntegral(denominator a))*denominator a))

computeRat :: [Integer] -> Rational
computeRat [x] = x%1
computeRat (a:as) = (a*numerator (computeRat as) + denominator (computeRat as))%numerator (computeRat as)

-- Question 4

root6 :: Double
root6 = sqrt 6

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

cf6 :: Int -> [Integer]
cf6 n = take n (2:cycle[2,4])

approxRoot6 :: Double -> Rational
approxRoot6 = apr6 1

apr6 :: Int -> Double -> Rational
apr6 i k = if abs(evalRat x - root6)<k
    then x
    else apr6 (i+1) k
 where x = computeRat(cf6 i)

-- Question 5 

mss :: [Int] -> Int
mss = bestIntermediate next 0
  where
    next s a = max 0 (s + a^3)

bestIntermediate :: Ord s => (s -> a -> s) -> s -> [a] -> s
bestIntermediate update within = maximum . scanl update within

-- Question 6

lps :: Eq a => [a] -> (Int, [a])
lps xs = arr ! (0,n-1) where
  n = length xs
  ax = listArray (0,n-1) xs
  arr = listArray ((0, 0),(n-1,n-1)) [f i j | i <- [0..(n-1)], j <- [0..(n-1)]]
  f i j
    | i > j = (0,[])
    | i == j = (1,[ax!i])
    | j == 0 && i/= n-1 = arr ! (i+1, j)
    | i == n-1 = arr ! (i ,j-1)
    | ax ! i == ax ! j = ( fst (arr ! (i+1,j-1)) + 2 , [ax!i]++ snd (arr ! (i+1,j-1)) ++[ax!i])
    | otherwise = if fst (arr ! (i+1, j)) > fst (arr ! (i ,j-1)) then
      arr ! (i+1, j) else arr ! (i,j-1)