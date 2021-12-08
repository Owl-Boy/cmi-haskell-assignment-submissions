import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )


 --Question 1
selectLeader :: Int -> Int -> Int
selectLeader 1 k = 1
selectLeader n k = newMod (1+ newMod (k-1+selectLeader (n-1) k) (n-1)) n

newMod :: Int -> Int -> Int
newMod a b
  | mod a b == 0 = b
  | otherwise    = mod a b

 --Question 2
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = lf [1..] 0 n n k where
  lf :: [Int] -> Int -> Int -> Int -> Int -> [[Int]]
  lf ls _ 1 _ _           = [[head ls]]
  lf ls pos count num dif = take count ls : lf ls' pos' (count - 1) num dif where
    ls'  = [x | x <- ls, mod x num /= mod (ls !! pos) num]
    pos' = mod ((mod pos count) + dif - 1) (count - 1)

 --Question 3
cf :: Rational -> [Integer]
cf r = helper a b 0 where
  helper n m 0 = div n m : helper m (mod n m) 1
  helper 1 m 1 = [m]
  helper m 1 1 = [m]
  helper n m 1 = div n m : helper m (mod n m) 1
  a            = numerator r
  b            = denominator r

computeRat :: [Integer] -> Rational
computeRat [i]    = i%1
computeRat (x:xs) = (x%1) + (1 / computeRat xs)

 --Question 4
root6 :: Double
root6 = sqrt 6

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational
approxRoot6 = helper 2.3 2.4 2.5 where
  helper a b c d
    | abs (root6 - evalRat b) < d = b
    | root6 - evalRat b <0        = helper a ((b+a)/2) b d
    | otherwise                   = helper b ((b+c)/2) c d

 --Question 5
mss :: [Int] -> Int
mss ls = helper ls' $ niceList $ compactList ls' where
   ls' = map (^ 3) ls
   helper :: [Int] -> [Int] -> Int
   helper ls [] = newMax ls
   helper ls [a] = a
   helper ls [a,b] = newMax [a,b,a+b]
   helper ls (a:b:c:cs) = if a+b > 0
                             then max a (helper ls ((a+b+c):cs))
                             else max a (helper ls (c:cs))

newMax :: Ord a => [a] -> a
newMax [a] = a
newMax (a:b:bs) = if max a b == a then newMax (a:bs) else newMax (b:bs)

compactList :: [Int] -> [Int]
compactList [a]      = [a]
compactList [a,b]    = if (a < 0) == (b < 0) then [a+b] else [a,b]
compactList (x:y:xs) = if (x<0)   == (y<0)
  then compactList ((x+y):xs) else x: compactList (y:xs)

niceList :: [Int] -> [Int]
niceList [a] = [a | a>0]
niceList [a,b] = niceList [a] ++ niceList [b]
niceList ls
  | head ls > 0 && last ls > 0 = ls
  | head ls < 0                = tail ls
  | otherwise                  = niceList $ removeLast ls where
      removeLast [x] = []
      removeLast (x:xs) = x: removeLast xs

 --Question 6
lps :: Eq a => [a] -> (Int, [a])
lps []                = (0,[])
lps as                = (len, seq) where
  (len, seq)          = lpsArr!(0,0)
  xs                  = as
  n                   = length as
  xArr                = listArray (0,n-1) xs
  yArr                = listArray (0,n-1) (reverse xs)
  lpsArr              = listArray ((0,0),(n,n)) [f i j | i <- [0..n], j <- [0..n]]
  f i j
    |i == n || j == n = (0,[])
    |xArr!i == yArr!j = (1+len1, (xArr!i) : seq1)
    |otherwise        = if len2 >= len3
                           then (len2, seq2)
                           else (len3, seq3) where
                             (len1, seq1) = lpsArr!(i+1,j+1)
                             (len2, seq2) = lpsArr!(i,j+1)
                             (len3, seq3) = lpsArr!(i+1,j)
