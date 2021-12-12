import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )

-- Question 1
selectLeader :: Int -> Int -> Int 
selectLeader n k = selection 0 n (k-1) [1..n] 
  
selection :: Int -> Int -> Int -> [Int] -> Int
selection _ _ _ [] = 0
selection _ _ _ [x] = x
selection i n k xs = selection ((i+k) `mod` (n-1)) (n-1) k [a|a<-xs, a/=xs!!i]

-- Question 2 
selectLeader' :: Int -> Int -> [[Int]] 
selectLeader' n k =  selection' 0 n (k-1) [1..n] 
  
selection' :: Int -> Int -> Int -> [Int] -> [[Int]]
selection' _ _ _ [] = []
selection' _ _ _ [x] = [[x]]
selection' i n k xs = xs : selection' ((i+k) `mod` (n-1)) (n-1) k ys  where ys = [a|a<-xs, a/=xs!!i]

-- Question 3
cf :: Rational -> [Integer]
cf p
        | denominator p == 1    = [numerator  p]
        | otherwise             = (numerator p `div` denominator p) : cf (np % dp)
                where   dp  = denominator num
                        np  = numerator num
                        num = denominator p % (numerator p `mod` denominator p)

computeRat :: [Integer] -> Rational
computeRat (x:xs) = reciprocal $ foldr f (0%1) (x:xs) 
                where   f :: Integer -> Rational -> Rational
                        f x p = denominator p % (x * denominator p + numerator p)
                        reciprocal :: Rational -> Rational
                        reciprocal p = denominator p % numerator p

-- Question 4
root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational 
approxRoot6 a = (2* denominator rat + numerator rat) % denominator rat 
        where   rat = finalRational a 0
                finalRational :: Double -> Rational -> Rational
                finalRational a p = if abs(evalRat p+2-root6) < a then p else finalRational a $ contfraccomp p 

contfraccomp :: Rational -> Rational 
contfraccomp p = (4*dp+np)%(9*dp+2*np)
                where   np = numerator p
                        dp = denominator p

-- Question 5 
mss :: [Int] -> Int
mss (x:xs) = maximum $ mss' $ map (^3) (x:xs)

mss' :: [Int] -> [Int]
mss' (x:xs) = x : zipWith f (mss' (x:xs)) xs
                where f a b = max (a+b) b

-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps xs = lcSubSeq (reverse xs) xs

lcSubSeq :: Eq a => [a] -> [a] -> (Int, [a])
lcSubSeq as bs = (n, reverse s) where
        (n, s) = last (last lcsTab)
        lcsTab = fstRow : zipWith nxtRow as lcsTab
        fstRow = replicate (length bs + 1) (0,[])
        nxtRow a r = (0,[]) : zipWith (g a) bs (zip3 r (tail r) (nxtRow a r))
        g a b ((m,s'),u,l)
                | a == b = (1+m, b:s')
                | otherwise = if fst u > fst l then u else l