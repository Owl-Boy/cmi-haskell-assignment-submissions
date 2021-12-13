import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )
import Data.List






-- Question 1
selectLeader :: Int -> Int -> Int
selectLeader n k = get n 1 [1..n] where
    get 1 1 [x] = x
    get m 1 ls = get (m - 1) k (tail ls)
    get m k' ls = get m (k' - 1) (tail ls ++ [head ls])







-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = get n 1 (zip [1..n] [1..n]) where
    get 1 1 [(_, x)] = [[x]]
    get m 1 ls = map snd (sortOn fst ls):get (m - 1) k (tail ls)
    get m k' ls = get m (k' - 1) (tail ls ++ [head ls])









-- Question 3
cf :: Rational -> [Integer]
cf s
    | denominator s == 1 = [numerator s]
    | otherwise = a : cf (denominator s % (numerator s - a*denominator s)) where
        a = numerator s `div` denominator s




computeRat :: [Integer] -> Rational
computeRat [] = 0
computeRat [x] = fromIntegral x
computeRat (x:xs) = fromIntegral x + denominator a % numerator a where
    a = computeRat xs









-- Question 4
root6 :: Double
root6 = sqrt 6

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)






approxRoot6 :: Double -> Rational
approxRoot6 del = computeRat (as root6 []) where
    as sq list = if abs (root6 - evalRat (computeRat (reverse list))) < del
        then reverse list
        else as (1/(sq - fromIntegral(floor sq))) (floor sq:list)











-- Question 5 
mss :: [Int] -> Int
mss = go 0 0 where
    go a max [] = max
    go a max (x:xs) = if a + x^3 >= 0
        then if a + x^3 > max then go (a + x^3) (a + x^3) xs else go (a + x^3) max xs
        else go 0 max xs









-- Question 6
lps :: Eq a => [a] -> (Int, [a])

--DP ATTEMPT BE LIKE
lps a = last (nRow len) where
    a' = listArray (1, len) a
    arev' = listArray (1, len) (reverse a)
    len = length a
    
    nRow 0 = replicate (len + 1) (0, [])
    nRow i = propagate $ update i 1 $ nRow (i-1) where
        update _ _ [x] = [x]
        update i j ((m, x):(m', y):xs)
            | a'!i == arev'!j = (m, x): (1+m, a'!i:x): tail (update i (j+1) ((m', y): xs))
            | otherwise = (m, x): update i (j+1) ((m', y):xs)

        propagate [x] = [x]
        propagate ((m, x): (m', y): xs)
            | m > m' = (m, x): propagate ((m, x): xs)
            | otherwise = (m, x): propagate ((m', y): xs)
