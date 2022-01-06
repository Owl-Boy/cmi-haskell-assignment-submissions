import Data.Ratio ( (%), denominator, numerator )
import Data.Array ( elems, listArray, array, (!) )
import Data.List ( sort )

-- Question 1
selectLeader :: Int -> Int -> Int
selectLeader 1 _ = 1
selectLeader n k = akkadBakkad [1..n] (k-1) where
    akkadBakkad [x] _ = x
    akkadBakkad (x:xs) k = akkadBakkad (reOrder xs k) k

reOrder :: [a] -> Int -> [a]
reOrder [x] _ = [x]
reOrder l 0 = l
reOrder (x:xs) k = reOrder (xs ++ [x]) (k-1)

-- Question 2 
selectLeader' :: Int -> Int -> [[Int]]
selectLeader' n k = tail $ reverse $ akkadBakkad [1..n] (k-1) [[]] where
    akkadBakkad [x] _ l = [x]:l
    akkadBakkad (x:xs) k l = akkadBakkad (reOrder xs k) k (sort (x:xs):l)

-- Question 3
cf :: Rational -> [Integer]
cf x = reverse (ulta (numerator x) (denominator x) []) where
    ulta a 1 l = a:l
    ulta a b l = ulta b (a `mod` b) ((a `div` b):l)

computeRat :: [Integer] -> Rational
computeRat [x] = toRational x
computeRat (x:xs) = toRational x + (denominator (computeRat xs) % numerator (computeRat xs))

-- Question 4
root6 :: Double
root6 = sqrt 6

evalRat :: Rational -> Double
evalRat x = fromIntegral (numerator x) / fromIntegral (denominator x)

approxRoot6 :: Double -> Rational
approxRoot6 = smol [2,2,4]

smol :: [Integer] -> Double -> Rational
smol (x:xs) e
    | abs (root6 - evalRat (computeRat (x:xs))) < e = computeRat (x:xs)
    | otherwise = smol (x:2:4:xs) e

-- Question 5 
mss :: [Int] -> Int
mss l
    | snd s < snd b = fst b - fst s
    | otherwise = max (fst b'- fst s) (fst b - fst s') where
        b = maximum z
        s = minimum z
        b' = maximum (drop (snd s) z)
        s' = minimum (take (snd b) z)
        z = zip (0 : coN (map (^3) l)) [0,1..]
        coN [x] = [x]
        coN [x,y] = [x,x+y]
        coN (x:y:xs) = x : coN ((x+y):xs)

-- Question 6
lps :: Eq a => [a] -> (Int, [a])
lps l
    | even k = (k, y ++ reverse y)
    | otherwise = (k, y ++ tail (reverse y)) where
        (k,y) = findPal l

findPal :: Eq a => [a] -> (Int, [a])
findPal [] = (0,[])
findPal [x] = (1, [x])
findPal (x:xs)
    | (x `elem` xs) && (fst l2 > fst l1) = l2
    | otherwise = l1 where
        l1 = findPal xs
        l2 = (2 + fst f, x:snd f)
        f = findPal (chop (reverse xs) x)
        chop (x:xs) y = if x == y then xs else chop xs y