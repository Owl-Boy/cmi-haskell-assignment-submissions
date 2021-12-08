import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!), Array )
import Data.List (sort)
import Data.Char
import System.Win32 (xBUTTON1, BY_HANDLE_FILE_INFORMATION (bhfiCreationTime))
-- Question 1
selectLeader :: Int -> Int -> Int 

lrem :: [Int] -> Int -> Int -> [Int]
lrem [] _ _ = []
lrem [x] _ _ = [x]
lrem (x:xs) _ 1 = xs
lrem (x:xs) a b
    |b>a = lrem (x:xs) a (if(b `mod` a == 0) then a else (b `mod` a))
    |otherwise = (lrem (xs++[x]) a (b-1))

helpp :: [Int] -> Int -> Int -> Int
helpp (x:xs) a b
    |a==1 = x
    |otherwise = helpp (lrem (x:xs) a b) (a-1) b

selectLeader n k
    |n>1 = helpp [2..n] (n-1) k
    |otherwise = 1

helpp' :: [Int] -> [[Int]] -> Int -> Int -> [[Int]]
helpp' (x:xs) c a b
    |a==1 = c
    |otherwise = helpp' (lrem (x:xs) a b) (c++([sort(lrem (x:xs) a b)])) (a-1) b

selectLeader' n k
    |n>1 = helpp' [2..n] ([[1..n], [2..n]]) (n-1) k
    |otherwise = [[1]]
-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]



-- Question 3

cf :: Rational -> [Integer]


cfhelp :: Rational -> [Integer] -> [Integer]
cfhelp 0 a = a 
cfhelp t b
    |r==t = s:b
    |otherwise = cfhelp (1/(t-r)) (s:b)
        where  
            s = floor t
            r = s%1 

cf x = reverse(cfhelp x [])


computeRat :: [Integer] -> Rational
computeRat [x] = x%1
computeRat (x:xs) = (fromIntegral x)+(1/(computeRat xs))




-- Question 4
root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational 

cfhelp' :: Double -> [Integer] -> Double -> Rational
cfhelp' t b a
    |not(null b) && (abs(evalRat(computeRat(reverse b))-root6)<a) = computeRat(reverse b)
    |otherwise = cfhelp' (1/(t-r)) (s:b) a
        where  
            s = floor t
            r = fromIntegral s

approxRoot6 a = cfhelp' root6 [] a

oo :: Double -> (Rational, Bool)
oo a = (r, abs(evalRat(r)-root6)<a) where r = approxRoot6 a

-- Question 5 
mss :: [Int] -> Int 
bruv :: [Int] -> [Int] -> [Int]
bruv x [] = x
bruv [] (y:ys) = bruv [y^3] ys
bruv (x:xs) (y:ys) = bruv ((x+y^3):x:xs) ys

bruvv :: [Int] -> [Int]
bruvv = bruv []

uwu :: [Int] -> Int -> Int -> Int
uwu [] a b = a-b

uwu (x:xs) a b
    |x<=a && b<=x = uwu xs a b
    |x>a = max (uwu xs x b) (a-x)
    |x<b = max (a-x) (uwu xs a b)
    |otherwise = -10000000000000

mss x = uwu r (head r) 0 where r = bruvv x 

-- Question 6
lps :: Eq a => [a] -> (Int, [a])

fib :: Int -> Int -> Int
fib a b = fibArr!(a,b) where 
    fibArr = listArray ((0,0),(a,b)) [f i j | i <- [0..a], j<- [0..b]]
    f 0 aa = aa
    f aa 0 = aa
    f x y = fibArr!(x-1,y) + fibArr!(x,y-1)

xArr a = listArray (0,length(a)-1) a

lps' woo = lps''!(0,n)    
    where
        n=length(woo)-1
        t=xArr(woo)        
        lps'' = listArray ((0,0),(n,n)) [uff i j| i<-[0..n], j<-[0..n]]
        uff i j
            |j<i = (0,[])
            |i==j = (1,[t!i])
            |t!i == t!j = (fst r + 2, (t!i):snd r)
            |otherwise = if(fst a1>fst a2) then a1 else a2
                where 
                    x = lps''
                    r=x!(i+1,j-1)
                    a1 = x!(i,j-1)
                    a2 = x!(i+1,j)


lps a = replicater (lps' a)
    where 
        replicater :: (Int, [a]) -> (Int, [a])
        replicater (n, a)
            |even n = (n, a ++ (reverse a))
            |otherwise = (n, a ++ tail(reverse(a)))