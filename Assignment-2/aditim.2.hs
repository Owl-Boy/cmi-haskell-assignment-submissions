import Data.List
-- Question 1

shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink [x] = [x]
shrink (x:xs)
    | x == head xs = shrink xs
    | otherwise = x : shrink xs

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f [] = []
shrink' f [x] = [x]
shrink' f (x:xs)
    | f x == f (head xs) = shrink' f (x:tail xs)
    | otherwise = x : shrink' f xs

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x,y) n = squares `intersect` allMoves (x,y) n

allMoves :: (Int, Int) -> Int -> [(Int, Int)]
allMoves (x,y) 0 = [(x,y)]
allMoves (x, y) n
    | (x,y) `elem` squares = shrink (sort (allMoves (x+2, y+1) (n-1) ++ allMoves (x+2, y-1) (n-1) ++
                                allMoves (x-2, y+1) (n-1) ++ allMoves (x-2, y-1) (n-1) ++
                                allMoves (x+1, y+2) (n-1) ++ allMoves (x+1, y-2) (n-1) ++
                                allMoves (x-1, y+2) (n-1) ++ allMoves (x-1, y-2) (n-1)))
    | otherwise = [] 

-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x,y) 0 = [(x,y)]
knightMove' (x,y) n = shrink (sort (knightMove (x,y) n ++ knightMove' (x,y) (n-1)))

-- Question 5

tuples :: [[a]] -> [[a]]
tuples [] = []
tuples [l] = [[x] | x <- l]
tuples (l:ls) = f l (tuples ls)

f :: [a] -> [[a]] -> [[a]]
f _ [] = []
f [] _ = []
f (x:xs) (l:ls) = map (x:) (l:ls) ++ f xs (l:ls)

-- Question 6

injTuples :: (Ord a, Eq a) => [[a]] -> [[a]]
injTuples ls = myFilter (tuples ls)

myFilter :: (Ord a, Eq a) => [[a]] -> [[a]]
myFilter [] = []
myFilter (l:ls)
    | myNub l == l = l : myFilter ls
    | otherwise = myFilter ls

-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

lineUps :: Ord a => [(Int, [a])] -> [[a]]
lineUps [] = []
lineUps [x] = enlist x
lineUps (x:xs) = g (enlist x) (lineUps xs)

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]

enlist :: Ord a => (Int, [a]) -> [[a]]
enlist (m,l) = pick m l

g :: Ord a => [[a]] -> [[a]] -> [[a]]
g _ [] = []
g [] _ = []
g (x:xs) (y:ys) = myFilter (map (x++) (y:ys) ++ g xs (y:ys))

pick :: Ord a => Int -> [a] -> [[a]]
pick 0 _ = []
pick _ [] = []
pick 1 l = [[x] | x <- l]
pick n l = myFilter' (myFilter (f l (pick (n-1) l)))

myFilter' :: (Ord a, Eq a) => [[a]] -> [[a]]
myFilter' [] = []
myFilter' (l:ls) = shrink (sort (map sort (l:ls)))