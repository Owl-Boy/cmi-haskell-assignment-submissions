import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!),Array )

-- Question 1
selectLeader :: Int -> Int ->Int
selectLeader n k =head ((\(y,z)->z) (arr ! (n-1)))
 where 
     rep :: Int -> [Int]
     rep i =  let 
                 (f,t) = arr ! (i-1)
                 u = (remove f t)
             in u
     loc :: Int -> Int
     loc i = let 
                 (f,t) = arr ! (i-1) 
             in  ((f  + k-1) `mod` (n-i))   
     remove :: Int -> [Int] ->[Int]
     remove i lis = (i `take` lis) ++ ((i+1) `drop` lis)
     arr ::Array Int (Int,[Int])
     arr = listArray (0,(n-1)) ((0,[1..n]):[(loc i  ,rep i) | i <- [1..(n-1)]])

-- Question 2 
selectLeader' :: Int -> Int ->[[Int]]
selectLeader' n k =map (\(y,z)->z) (elems arr)
 where 
     rep :: Int -> [Int]
     rep i =  let 
                 (f,t) = arr ! (i-1)
                 u = (remove f t)
             in u
     loc :: Int -> Int
     loc i = let 
                 (f,t) = arr ! (i-1) 
             in  ((f  + k-1) `mod` (n-i))   
     remove :: Int -> [Int] ->[Int]
     remove i lis = (i `take` lis) ++ ((i+1) `drop` lis)
     arr ::Array Int (Int,[Int])
     arr = listArray (0,(n-1)) ((0,[1..n]):[(loc i  ,rep i) | i <- [1..(n-1)]])
-- Question 3
cf :: Rational -> [Integer]
cf rat = if q==1 then [p] else ((p `div`q) : cf (q % (p-q*(p `div` q)))) where 
    p = numerator rat 
    q = denominator rat
computeRat :: [Integer] -> Rational
computeRat [] = error "empty list how to calculate!!"
computeRat [i] = (i%1)
computeRat (x:xs) =  let (p,q) = (numerator (computeRat xs),denominator (computeRat xs)) in ((x*p +q)%p)                                         

-- Question 4
root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational 
approxRoot6 e = computeRat (fp (\x->abs (evalRat((computeRat x))-root6)<=e) tab)
 where 
     gen :: Int -> [Integer] 
     gen i = if i `mod` 2 ==1 then (tab!!(i-1) ++ [2]) else (tab !!(i-1) ++[4])
     tab :: [[Integer]]  
     tab =  [2] :[gen i | i <- [1..]]
     fp :: (a->Bool) -> [a] ->a
     fp p (x:xs) = if p x == True then x else fp p xs
-- Question 5 
mss :: [Int] -> Int 
mss [] =error "Empty Set "
mss lis = row !! (n-1)
 where 
     n = length lis
     u= tail (reverse lis)
     rowa= (lis !! (n-1))^3 : (zipWith (\x y -> x^3 + (max (0) y)) (u) (rowa))
     row = (lis !! (n-1))^3 : (zipWith (max) row (tail(rowa))) 
-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps [] = (0,[])
lps lis =arr0 ! (0,(n-1))  
 where
     n = length lis
     maxp :: Eq a => a -> [a] -> Int 
     maxp i l = ((length l)-1)-(ind i (reverse l)) 
     ind :: Eq a => a -> [a] -> Int
     ind i (x:xs) = if i==x then 0 else 1+(ind i xs)
     cut i j = (i) `drop` ((j+1) `take` lis )
     l0 i j =if i>j then (0,[]) else if i==j then (1,[lis !! i])  else if fst(arr1 ! (i,j))>=(fst(arr0 ! (i+1,j))) then (arr1 ! (i,j)) else (arr0 ! (i+1,j))
     l1 i j =if i>j then (0,[]) else if i==j then (1,[lis !! i])  else let(x,xs)=((lis !! i),(cut (i+1) j)) in (if x `elem` xs then (2+fst(arr0 ! (i+1,(i+ maxp x xs))),x:snd(arr0 ! (i+1,(i+ maxp x xs)))++[x]) else (1,[x]))
     arr0 = listArray ((0,0),((n-1),(n-1))) [l0 i j | i <-[0..(n-1)] , j <-[0..(n-1)]]
     arr1 = listArray ((0,0),((n-1),(n-1))) [l1  i j | i <-[0..(n-1)] , j <-[0..(n-1)]]
