import Data.Array (array, elems, listArray, (!))
import Data.Ratio (denominator, numerator, (%))

-- Question 1
selectLeader :: Int -> Int -> Int
selectLeader size = helper 0 [1 .. size]
  where
    helper index (x : xs) step
      | null xs = x
      | otherwise = helper (index + 1) (xs ++ [x | index `mod` step /= 0]) step

-- Question 2
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' size step = init $ helper 0 [1 .. size] step
  where
    helper index ls step
      | length ls == 1 = [ls]
      | otherwise =
        let newLs = [ls !! i | i <- [0 .. (length ls - 1)], i /= index]
         in newLs : helper ((index - 1 + step) `mod` length newLs) newLs step

-- Question 3
cf :: Rational -> [Integer]
cf num
  | denominator num == 1 = [numerator num]
  | otherwise = floor num : cf (1 / (num - floor num % 1))

computeRat :: [Integer] -> Rational
computeRat (x : xs)
  | null xs = x % 1
  | otherwise = x % 1 + 1 / computeRat xs

-- Question 4
root6 :: Double
root6 = sqrt 6

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational
approxRoot6 = computeRat . helper [floor root6]
  where
    helper intList delta
      | abs (current - root6) <= delta = intList
      | (current - root6) * (next - root6) > 0 = helper nextList delta
      | otherwise = helper (intList ++ [1]) delta
      where
        current = evalRat $ computeRat intList
        nextList = init intList ++ [last intList + 1]
        next = evalRat $ computeRat nextList

-- Question 5
mss :: [Int] -> Int
mss (x : xs) = helper (x ^ 3) (x ^ 3) xs
  where
    helper finalVal candidateVal ls
      | finalVal < candidateVal = helper candidateVal candidateVal ls
      | null ls = finalVal
      | candidateVal <= 0 = helper finalVal (head ls ^ 3) (tail ls)
      | otherwise = helper finalVal (candidateVal + head ls ^ 3) (tail ls)

-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps ls = lcs ls (reverse ls)
  where
    lcs as bs = (n, reverse s)
      where
        (n, s) = last (last lcsTab)
        lcsTab = fstRow : zipWith nxtRow as lcsTab
        fstRow = replicate (length bs + 1) (0, [])
        nxtRow a r =
          (0, []) :
          zipWit
            (g a)
            bs
            (zip3 r (tail r) (nxtRow a r))
        g a b ((m, s'), u, l)
          | a == b = (1 + m, b : s')
          | otherwise = if fst u > fst l then u else l

-- quicksort eh
quicksort :: Ord a -> [a] -> [a]
quicksort ls
  | length ls <= 1= ls
  | otherwise = quicksort ls1 ++ [pivot] ++ quicksort ls2
  where
    pivot = head ls
    ls1 = [x| x<- (tail ls), x < pivot]
    ls1 = [x| x<- (tail ls), x >= pivot]
