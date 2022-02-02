import Data.Array (array, elems, listArray, (!))
import Data.Ratio (denominator, numerator, (%))

-- Question 1
nextList :: [Int] -> Int -> Int -> ([Int], Int)
nextList [] _ _ = error "nextList called with empty list"
nextList xs k pos = (xs', pos')
  where
    xs' = take pos xs ++ drop (pos + 1) xs
    pos' = (pos - 1 + k) `mod` length xs'

selectLeader :: Int -> Int -> Int
-- using 0 based indexing to preserve sanity
selectLeader n k = realSelectLeader [1 .. n] 0
  where
    realSelectLeader :: [Int] -> Int -> Int
    realSelectLeader [] _ = error "Should never reach this"
    realSelectLeader xs pos
      | length xs == 1 = head xs
      | otherwise = realSelectLeader xs' pos'
      where
        (xs', pos') = nextList xs k pos

-- Question 2
selectLeader' :: Int -> Int -> [[Int]]
-- using 0 based indexing to preserve sanity
selectLeader' n k = realSelectLeader' [1 .. n] 0
  where
    realSelectLeader' :: [Int] -> Int -> [[Int]]
    realSelectLeader' [] _ = error "Should never reach this"
    realSelectLeader' xs pos
      | length xs == 1 = [xs]
      | otherwise = xs : realSelectLeader' xs' pos'
      where
        (xs', pos') = nextList xs k pos

-- Question 3
cf :: Rational -> [Integer]
cf r = if r == 0 then [0] else flipContinue (1 / r)
  where
    flipContinue :: Rational -> [Integer]
    flipContinue r =
      if numerator r == 1
        then [denominator r]
        else first : flipContinue (rest % numerator r)
      where
        (first, rest) = divMod (denominator r) (numerator r)

computeRat :: [Integer] -> Rational
computeRat xs = foldr1 f [x % 1 | x <- xs]
  where
    f :: Rational -> Rational -> Rational
    f n1 n2 = n1 + (1 / n2)

-- Question 4
root6 :: Double
root6 = sqrt 6

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

root6List :: [Integer]
root6List = 2 : cycle [2, 4]

approxRoot6 :: Double -> Rational
approxRoot6 epsilon = takeMore 1
  where
    takeMore :: Int -> Rational
    takeMore n =
      if abs (root6 - evalRat root6Rat) < epsilon
        then root6Rat
        else takeMore (n + 1)
      where
        root6Rat = computeRat (take n root6List)

-- Question 5
mss :: [Int] -> Int
mss [] = error "the set of sums is empty, so the maximum is undefined"
mss l = answer
  where
    (_, _, answer) = foldr s (0, 0, minBound) l
    s :: Int -> (Int, Int, Int) -> (Int, Int, Int)
    -- minSumRight gives us the best right index to use to maximise the sum
    s v (sumRight, minSumRight, answer) =
      (sumRight', min minSumRight sumRight', max answer currentAnswer)
      where
        sumRight' = sumRight + (v ^ 3)
        currentAnswer = sumRight' - minSumRight

-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps [] = (0, [])
lps as = (len, seq)
  where
    (len, seq) = lpsArr ! (1, n)
    n = length as
    aArr = listArray (1, n) as
    lpsArr =
      array
        ((1, 1), (n, n))
        [((i, j), f i j) | i <- [n, n - 1 .. 1], j <- [n, n - 1 .. 1]]
    f i j
      | i > j = (0, [])
      | i == j = (1, [aArr ! i])
      | ai == aj = (lendl + 2, ai : (seqdl ++ [ai]))
      | lend > lenl = (lend, seqd)
      | otherwise = (lenl, seql)
      where
        ai = aArr ! i
        aj = aArr ! j
        (lend, seqd) = lpsArr ! (i + 1, j)
        (lenl, seql) = lpsArr ! (i, j - 1)
        (lendl, seqdl) = lpsArr ! (i + 1, j - 1)
