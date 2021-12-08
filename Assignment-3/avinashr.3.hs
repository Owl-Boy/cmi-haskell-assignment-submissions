import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )

-- Question 1
selectLeader :: Int -> Int -> Int
selectLeader n m =  fst(fnc [1..n] m 1 [zip [1..n] [1..n]])

fnc :: [Int] -> Int -> Int -> [[(Int,Int)]] -> (Int,[[(Int,Int)]])
fnc [x] s p lst = (x,lst)
fnc xs s p lst = fnc (map snd (filter (\(a,_) -> (a-p) `mod` s /= 0 ) (zip [1..length xs] xs))) s (s - (length xs - p) `mod` s) (lst ++ concatMap (\(a,_)-> f p s a xs ) (zip [1..length xs] xs))
    where f :: Int -> Int -> Int -> [Int] -> [[(Int,Int)]]
          f p s a xs
            |a <= 0 = [zip [1..length xs] xs]
            |(a-p) `mod` s /= 0 = []
            |otherwise = [takeWhile (\(x,_) -> x < a) (head(f p s (a-s) xs)) ++ dropWhile (\(x,_) -> x <= a) (head(f p s (a-s) xs))]

-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n m = map (map snd) ( snd (fnc [1..n] m 1 [zip [1..n] [1..n]]) )

-- Question 3
cf :: Rational -> [Integer]
cf x= if r == 0 then [d] else d:cf (q%r)
    where (p,q) = (numerator x,denominator x)
          (d,r)= divMod p q

computeRat :: [Integer] -> Rational
computeRat [x] = x%1
computeRat (x:xs) = x%1 + denominator (computeRat xs) % numerator (computeRat xs)

-- Question 4
root6 :: Double
root6 = sqrt 61

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

rt6mns2 :: [Integer]
rt6mns2 = 2:4:rt6mns2

approxRoot6 :: Double -> Rational
approxRoot6 x = f 1
    where
        f :: Int -> Rational
        f n = if abs (evalRat (computeRat (take n (2:rt6mns2))) - root6) < abs x then computeRat (take n (2:rt6mns2)) else f (n+1)

-- Question 5
mss :: [Int] -> Int
mss xs =  maximum [arr_3!b - arr_3!a| a <- ls_l, b <- filter (> a) ls_u]  where
    arr   = listArray (1, length xs) ([x^3|x<-xs])
    arr_3 = listArray (0, length xs ) (0:[arr!i + arr_3!(i-1)|i<-[1..(length xs )]])
    ls_u  = length xs:[idx | idx <- [1..(length xs -1)], arr ! idx >0 , arr ! (idx + 1) < 0 ]
    ls_l  = 0:1:length xs:[idx' | idx' <- [2..(length xs-1)], arr ! (idx'+1) >0 , arr ! idx'  < 0 ]

-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps xs
    |null ls = (1,[head xs])
    |even len=  if k xs lst (len `div` 2) == lst!!(len `div` 2) then (len,lst) else (len + 1,take (len `div` 2) lst ++ [k xs lst (len `div` 2)] ++ drop (len `div` 2) lst ) 
    |otherwise = (len ,take (len `div` 2) lst ++ [k xs lst (len `div` 2 + 1)] ++ drop (len `div` 2 + 1) lst )where
    len=length (snd(table!(0,n)))
    lst= snd(table!(0,n))
    table = array ((0, 0),(n,n)) ([((i,j),f' i j )|i<-[0..n],j<-[i..n]])
    f' i j
        |i == j           = (1, [u])
        |j - i  == 1 && arr'!i == arr'!j = (2,[u,u])
        |u == v = (2 + fst r, u:snd r ++ [u])
        |otherwise = if fst p > fst q then p else q
        where (p,q,r,u,v) = (table!(i+1,j), table!(i,j-1), table!(i+1,j-1), arr'!i, arr'!j)
    arr' = listArray (0, n) ls
    n = length ls -1
    ls = fnt [] xs 

fnt :: Eq a => [a] -> [a] -> [a]
fnt ys [] = reverse ys
fnt ts (y:ys) = if y `notElem` ys && y `notElem` ts then fnt ts ys else  fnt (y:ts) ys

k :: Eq a => [a] -> [a] -> Int -> a
k (a:as) (b:bs) j
    |length bs + 1 == j =  a
    |otherwise = if a==b then k as bs j else k as (b:bs) j 

