import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )

quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (p:xs) = (quicksort lesser) ++ [p] ++ (quicksort greater) where
   lesser = filter (< p) xs
   greater = filter (>= p) xs

quicksortWith :: [(Int,a)] -> [(Int,a)]
quicksortWith [] = []
quicksortWith ((n,l):xs) = (quicksortWith lesser) ++ [(n,l)] ++ (quicksortWith greatereq) where
   lesser = [(n1,x) | (n1,x) <- xs, n1 < n]
   greatereq = [(n1,x) | (n1,x) <- xs, n1 >= n]

-- Question 1

switchList' :: [a] -> Int -> [a]
switchList' l k 
   | k > length l  = switchList'' l (minimum [a | a <- [1..(length l)], (a - k) `mod` (length l) == 0])
   | otherwise     = l2 ++ (tail l1) where 
      (l1,l2) = splitAt k l

switchList'' :: [a] -> Int -> [a]
switchList'' l k = l2 ++ (init l1) where 
      (l1,l2) = splitAt k l


selectLeader :: Int -> Int -> Int 
selectLeader 0 _ = undefined
selectLeader 1 _ = 1
selectLeader n k = listLeader [1..n] k where
   listLeader [a] _ = a
   listLeader a k = listLeader (switchList' a k) k


-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = [1..n] : (map quicksort (sLists [1..n] k)) where
   sLists [a] _ = []
   sLists l k = (switchList' l k) : sLists (switchList' l k) k

-- Question 3

cf :: Rational -> [Integer]
cf q = if snd (fracPart q) == 0 then [fst (fracPart q)] else [fst (fracPart q)] ++ cf (1/(snd (fracPart q)))

fracPart :: Rational -> (Integer,Rational)
fracPart a = if a>0 then fracFunc (0,a) else fracFunc' (0,a) where
   fracFunc (n,q)
      | q < 1 && q >= 0      = (n,q)
      | otherwise            = fracFunc (n+1,q-1)
   fracFunc' (n,q)
      | q < 1 && q >= 0      = (n,q)
      | otherwise            = fracFunc' (n-1,q+1)

computeRat :: [Integer] -> Rational
computeRat [] = undefined
computeRat [a] = fromInteger a
computeRat (x:xs) = fromInteger x + (1/(computeRat xs))


-- Question 4

root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

listRoot6 :: [Integer]
listRoot6 = 2:z where
   z = [2,4] ++ z

listApprox :: [Rational]
listApprox = takeList 1 where
   takeList r = (computeRat (take r listRoot6)): takeList (r+1)

approxRoot6 :: Double -> Rational 
approxRoot6 eps = takeFrom listApprox eps where
   takeFrom (q:qs) epsilon = if abs (evalRat q - root6) < epsilon then q else takeFrom qs epsilon


-- Question 5


mss :: [Int] -> Int
mss l = mss' l 0 0 where
   mss' [] a m = m
   mss' xs'@(x:xs) a m
      | a >= 0 && m >= x^3+a   = mss' xs (x^3+a) m
      | a >= 0 && m < x^3+a    = mss' xs (x^3+a) (x^3+a)
      | a < 0  && m >= x^3     = mss' xs (x^3) m
      | a < 0  && m < x^3      = mss' xs (x^3) (x^3)
{-
mss xs = maximum [(r i j) | i <- [1..n], j <- [1..n], i <= j] where
   r i j = cubeSum (take (j+1-i) (snd (splitAt (i-1) xs)))
   n = length xs
   cubeSum l
      | length l == 0    = 0
      | otherwise        = (head l)^3 + cubeSum (tail l) -}


-- Question 6
elemPosExtr :: Eq a => [a] -> a -> (Int,[a]) 
elemPosExtr [] x = (0,[])
elemPosExtr xs x = if x==(last xs) then (length xs, init xs) else elemPosExtr (init xs) x


lps :: Eq a => [a] -> (Int, [a])
lps l = last (quicksortWith palinList) where
   palinList = [(length (f i), f i) | i <- [0..n]]
   f = palinSet l
   n = length l

palinSet :: Eq a => [a] -> Int -> [a]
palinSet l i = palinSet' (snd (splitAt i l)) [] where
   palinSet' [] l = (reverse l) ++ l
   palinSet' [a] l = (reverse l) ++ a:l
   palinSet' xs'@(x:xs) l = if fst (elemPosExtr xs x) == 0 then palinSet' xs l else palinSet' (snd (elemPosExtr xs x)) (x:l)
