nextSquare :: Integer -> Integer
nextSquare n = head ([i ^ 2 | i <- [0 ..], i ^ 2 >= n])

previousCube :: Integer -> Integer
previousCube n = (head ([i | i <- [0 ..], i ^ 3 > n]) - 1) ^ 3

len :: Integer -> Integer
len 0 = 0
len s = len (s `div` 10) + 1

revToList :: Integer -> [Integer]
revToList 0 = []
revToList n = (n `mod` 10) : revToList (n `div` 10)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []

halvethelist :: [Integer] -> [Integer]
halvethelist s = take ((length s + 1) `div` 2) s

f :: Integer -> [Integer]
f s
  | even (len s) = x s ++ reverse' (x s)
  | otherwise = x s ++ tail (reverse' (x s))
  where
    x s = halvethelist (reverse' (revToList s))

g :: [Integer] -> Integer
g [] = 0
g (x : xs) = (10 * g xs) + x

nextPalin :: Integer -> Integer
nextPalin s
  | s <= 0 = 0
  | g (f s) >= s = g (f s)
  | otherwise = g (f (s + 10 ^ (len s `div` 2)))

sievePrimes :: [Integer] -> [Integer]
sievePrimes [] = []
sievePrimes (x : xs) = x : sievePrimes [a | a <- xs, (a `mod` x) /= 0]

primesIn :: Integer -> Integer -> [Integer]
primesIn l u
  | u <= 1 = []
  | l > u = []
  | otherwise = [p | p <- sievePrimes [2 .. u], p >= l, p <= u]

smally :: Integer -> Integer -> Integer
smally n k
  | k ^ 2 > n = n
  | n `mod` k == 0 = k
  | otherwise = smally n (k + 1)

isPrime :: Integer -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = smally n 2 == n

decToBin :: Integer -> Integer
decToBin n = if n > 0 then 10 * decToBin (n `div` 2) + (n `mod` 2) else 0

binToDec :: Integer -> Integer
binToDec n
  | n == 0 = 0
  | otherwise = 2 * binToDec (n `div` 10) + (n `mod` 2)
