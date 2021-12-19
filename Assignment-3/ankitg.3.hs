import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )
import Data.List ( sort )

--Question 1
selectLeader :: Int -> Int -> Int
selectLeader n k = head (select [2..n] k) 

mydel:: Int -> [Int]-> [Int]
mydel n [] =  []
mydel 0 (x:xs) = xs
mydel n (x:xs) 
   | n<=length (x:xs) = drop n (x:xs) ++ take (n-1) (x:xs)
   | n `mod` length(x:xs)==0 = init (x:xs)
   | otherwise = drop b (x:xs) ++ take (b-1) (x:xs) where
       b = n `mod` length(x:xs)
select :: [Int] -> Int -> [Int]
select [x] k = [x]
select (x:xs) k = select (mydel k  (x:xs)) k

--Question 2
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k  = init([1..n]:[2..n]:select' [2..n] k)
select' :: [Int]-> Int ->[[Int]]
select' [x] k = [[x]]
select' (x:xs) k = sort( mydel k (x:xs)) : select' (mydel k (x:xs)) k

--Question 3
cf :: Rational -> [Integer]
cf 0 = [0]
cf x
   |fromIntegral(floor x )== x = [round x]
   |otherwise= floor x: cf (1/(x- fromIntegral(floor x)))

computeRat' :: [Integer] -> Rational
computeRat' [x] = fromIntegral x
computeRat' (x:xs) = fromIntegral x + (1/ computeRat' xs)

computeRat :: [Integer]-> Rational 
computeRat (x:xs) = numerator (computeRat' (x:xs)) % denominator ( computeRat' (x:xs))

--Question 4
root6 :: Double 
root6 = sqrt 6 

list6' :: [Integer]
list6' = [2,4]++list6'

list6 :: [Integer]
list6 = 2:list6'
evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational
approxRoot6 e =  computeRat (  head [ take i list6 | i<-[1..], abs(root6-evalRat(computeRat (take i list6 )))<e ])

--Question 5

mss :: [Int] -> Int
mss as = maximum $ elems arr2 where
   n= length as
   arr = listArray (0,n-1) (map (^3) as)
   arr2 = listArray (0,n-1) [ localmax i |i<-[0..n-1]] where
      localmax i 
       | i==0 = arr ! 0
       | otherwise = max  (arr ! i)  ((arr2 ! (i-1))+ (arr ! i))
       


--Question 6

lpsnaive :: Eq a => [a] -> (Int, [a])
lpsnaive [] = (0,[])
lpsnaive [x] = (1,[x])
lpsnaive (x:xs) 
 | x==last xs = (2+ fst ks , x:snd ks ++ [last xs])
 | otherwise = if len1 > len2
    then lpsnaive xs
    else lpsnaive $ init (x:xs) where
       (len1,seq1) =  lpsnaive xs
       (len2,seq2) =  lpsnaive $ init (x:xs)
       ks = lpsnaive (init xs)




lps :: Eq a => [a] -> (Int, [a])
lps [] =(0,[])
lps [x] = (1,[x])
lps (x:xs) = arr2 ! (0,n-1) where



   n = length (x:xs)
   arr = listArray (0,n-1) (x:xs)
   arr2 = array ((0,0),(n,n)) ((concat[[ ((i,j), f i j)|i<-[0..n-1] ,j<-[0..n-1], (j-i)== k] | k<-[1..n-1]] )++[ ((i,j),f i j)| i<-[0..n-1] ,j<-[0..n-1],i>=j])
   f i j
      | i==j = (1,[arr!i])
      | i>j  = (0,[])
      | otherwise = if arr!i==arr!j
         then (2 + fst(arr2 ! (i+1,j-1)),arr!i: snd(arr2 ! (i+1,j-1)) ++ [arr!j])
         else if  fst( arr2 ! (i+1,j))> fst(arr2 ! (i,j-1))
            then arr2 ! (i+1,j)
            else arr2 ! (i,j-1)
   