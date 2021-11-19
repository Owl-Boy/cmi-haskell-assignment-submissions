import Data.List ( sort, sortOn )
-- Question 1

shrink :: Eq a => [a] -> [a]
shrink []  = []
shrink [a] = [a]
shrink (x:y:xs)
    |x == y    = shrink (x:xs)
    |otherwise = x: shrink (y:xs)

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f []  = []
shrink' f [a] = [a]
shrink' f (x:y:xs)
    |f x == f y =   shrink' f (x:xs)
    |otherwise  = x:shrink' f (y:xs)

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (a,b) n = foldr (\k l -> shrink(sort(filter (`elem` squares) (concatMap (\(x,y) -> [(x+2,y+1),(x+2,y-1),(x-2,y+1),(x-2,y-1),(x+1,y+2),(x+1,y-2),(x-1,y+2),(x-1,y-2)]) l)))) [(a,b)] [1..n]

-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (a,b) n = concatMap ((a,b) `knightMove`) [0..n]

-- Question 5

tuples :: [[a]] -> [[a]]
tuples = foldr (\x l -> concatMap (\i -> map ((x!!i) :) l ) [0..(length x - 1)] ) [[]]

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples l = filter f' (tuples l)
-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps [] = [[]]
lineUps (l:ls)  =  filter f' $ concatMap (\n -> map (\l' ->fnc l!!n ++ l') (lineUps ls) ) [0..(length(fnc l) - 1)]

fnc ::  Eq a => (Int, [a]) -> [[a]]
fnc (n,l)
    |n == 0        = [[]]
    |n == length l = [l]
    |otherwise     = concatMap (\i -> map (\l' -> (l!!i):l') (fnc (n-1, drop (i+1) l)) ) [0..(length l - n)]

f' :: Eq a => [a] -> Bool
f' l = foldr (\n b -> not ( l!!n `elem` drop (n+1) l) && b) True [0..(length l-1)]


fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
