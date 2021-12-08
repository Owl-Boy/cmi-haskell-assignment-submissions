import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )

-- Question 1
selectLeader :: Int -> Int -> Int 

-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]

-- Question 3
cf :: Rational -> [Integer]

computeRat :: [Integer] -> Rational

-- Question 4
root6 :: Double 
root6 = sqrt 6 

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational 

-- Question 5 
mss :: [Int] -> Int 

-- Question 6
lps :: Eq a => [a] -> (Int, [a])
