import Data.Ratio ( (%), denominator, numerator)
import GHC.Real (infinity)
import Data.Array (elems, listArray, array, (!),range)
import Data.List (delete,(\\),tails,inits,maximumBy)
import Data.Function (on)
 
-- Question 1

elim_seq :: Int -> Int -> [Int]
elim_seq n k = f (n-1) [2 .. n]
  where
    f 0 _ = []
    f m s = x : f (m - 1) (right ++ left) -- wrap around the previous part
      where
        (left, x:right) = splitAt (mod (k - 1) m) s
 
selectLeader :: Int -> Int -> Int 
selectLeader n k = head . last $ selectLeader' n k

-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = init $ [1..n] : (map ([2..n] \\) $ inits $ elim_seq n k)

-- Question 3
cf :: Rational -> [Integer]
cf n = (p`div`q) : cf' (p`mod`q) q 
  where 
    (p,q) = (numerator n, denominator n)
    cf' :: Integer -> Integer -> [Integer]
    cf' a b
        | b== 0 = []
        | a < b = (cf' b a)
        | otherwise = (a `div` b) : (cf' (a`mod`b) b) 

computeRat :: [Integer] -> Rational
computeRat = foldr (\a b -> fromIntegral a + 1/b) infinity 

-- Question 4
continued = 2 : cycle [2,4]

root6 :: Double 
root6 = sqrt 6 

approx_to_precision n = computeRat (take n continued)

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational 
approxRoot6 eps = head $ dropWhile (\r -> abs (root6 - evalRat r) > eps) $ map approx_to_precision [1..]

-- Question 5 

f = foldr (\a b -> 0 `max` (a+b)) 0
mss :: [Int] -> Int 
mss = maximum . map f . tails . map (^3)

-- Question 6
 
lps :: Eq a => [a] -> (Int,[a])
lps seq = subseqs ! (0,n-1)
  where
    n = length seq
    seqs = listArray (0,n-1) seq
    subseqs = listArray bounds [lps_aux i j | (i,j) <- range bounds]
    bounds=((0,0),(n-1,n-1))
    lps_aux i j
        | i == j = (1, [seqs ! i])
        | i > j = (0,[])
        | seqs ! i == seqs ! j = (2 + fst mid, [seqs ! i] ++ snd mid ++ [seqs ! i])
        | otherwise = (max (fst c) (fst d), comp (snd c) (snd d))
      where
        mid = subseqs ! (i+1, j-1)
        (c,d) = (subseqs ! (i,j-1), subseqs ! (i+1,j))
        comp x y = if length x > length y then x else y
