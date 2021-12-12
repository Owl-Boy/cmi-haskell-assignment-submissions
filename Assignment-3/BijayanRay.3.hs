import Data.Array (array, elems, listArray, (!))
import Data.Ratio (denominator, numerator, (%))

-- Question 1

selectLeader :: Int -> Int -> Int
selectLeader 1 _ = 1
selectLeader n k = sl [2 .. n] k
  where
    sl [x] _ = x
    sl l k = sl (a ++ init b) k
      where
        (b, a) = if k `mod` length l /= 0 then splitAt (k `mod` length l) l else (l, [])

-- Question 2

selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = [1 .. n] : [2 .. n] : sl [2 .. n] [2 .. n] k
  where
    sl [x] _ _ = []
    sl l ls k = c : sl (a ++ init b) c k
      where
        c = remv (last b) ls
          where
            remv _ [] = []
            remv y (s : ys) = if y == s then remv y ys else s : remv y ys
        (b, a) = if k `mod` length l /= 0 then splitAt (k `mod` length l) l else (l, [])

-- Question 3

cf :: Rational -> [Integer]
cf x = if a `mod` b /= 0 then a `div` b : cf (b % (a `mod` b)) else [a]
  where
    a = numerator x
    b = denominator x

computeRat :: [Integer] -> Rational
computeRat [l] = toRational l
computeRat (l : ls) = (l % 1) + denominator (computeRat ls) % numerator (computeRat ls)

-- Question 4

root6 :: Double
root6 = sqrt 6

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational
approxRoot6 e = go (2 : l) e 1
  where
    l = 2 : 4 : l

    go ls e i = if abs (evalRat c - root6) < e then c else go ls e (i + 1)
      where
        c = computeRat (take i (2 : l))

-- Question 5

mss :: [Integer] -> Integer
mss l = maximum [arr ! i | i <- [0 .. (n - 1)]]
  where
    n = length l
    arr = listArray (0, n - 1) [mss' i ls | i <- [0 .. n - 1]]
    mss' a ls = if a == (n - 1) then ls ! (n - 1) else max (ls ! a) (ls ! a + arr ! (a + 1))
    ls = listArray (0, n - 1) (map (^ 3) l) --O(n)

-- mss l = maximum [mss' i (map (^3) l)|i<-[0..(length l-1)]]

--  where mss' a ls = maximum(scanl (+) (ls !! a) (drop (a+1) ls)) -- O(n^2) but I included it cuz I find it super cute.

-- Question 6

lps :: Eq a => [a] -> (Int, [a])
lps l = lcs1 l (reverse l)

lcs1 :: (Eq a, Ord b, Num b) => [a] -> [a] -> (b, [a])
lcs1 as bs = (z, reverse s)
  where
    (z, s) = lcst ! (n, m)
    (n, m) = (length as, length bs)
    lsta = listArray (1, n) as
    lstb = listArray (1, m) bs
    lcst = listArray ((0, 0), (n, m)) [f x y | x <- [0 .. n], y <- [0 .. m]]
      where
        f 0 _ = (0, [])
        f _ 0 = (0, [])
        f x y
          | lsta ! x == lstb ! y = (k + 1, lsta ! x : l)
          | otherwise = if fst u > fst d then u else d
          where
            (k, l) = lcst ! (x - 1, y - 1)
            u = lcst ! (x, y - 1)
            d = lcst ! (x - 1, y)
