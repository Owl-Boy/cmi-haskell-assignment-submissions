import Data.List (sortOn, sort)

-- Question 1

shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink [y] = [y]
shrink (x:xs) = if x == head xs then shrink xs
                                else x : shrink xs

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' _ [] = []
shrink' _ [y] = [y]
shrink' f (x:xs) = if (f x) == (f (head xs)) then shrink' f (x : (tail xs))
                                             else x : shrink' f xs

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x, y) 0 = [(x, y)]
knightMove (x, y) n = (shrink . sort . concat) [knightMoveHelper (a, b) | (a, b) <- knightMove (x, y) (n-1)]

knightMoveHelper :: (Int, Int) -> [(Int, Int)]
knightMoveHelper (x, y) = filter (\(a, b) -> (a, b) `elem` squares) [(x+2, y+1), (x+2, y-1), (x-2, y-1), (x-2, y+1), (x+1, y+2), (x+1, y-2), (x-1, y+2), (x-1, y-2)]

-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' t 0 = [t]
knightMove' s n = shrink $ mergeSorted (knightMove s n) (knightMove' s (n-1))

mergeSorted :: Ord a => [a] -> [a] -> [a]
mergeSorted [] x = x
mergeSorted y [] = y
mergeSorted (l:ls) (m:ms) = if l >= m then m : mergeSorted (l:ls) ms
                                      else l : mergeSorted ls (m:ms)

-- Question 5

tuples :: [[a]] -> [[a]]
tuples [] =[]
tuples [m] = [[x] | x <- m]
tuples (l:ls) = [n : tprev | n <- l, tprev <- tuples ls]

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples [] = []
injTuples [m] = [[x] | x <- m]
injTuples (l:ls) = [n : tprev | n <- l, tprev <- tuples ls, not (n `elem` tprev)]

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
lineUps [] = []
lineUps [(0, m)] = []
lineUps [(1, k)] = [[x] | x <- k]
lineUps (l:ls) = if (fst l == 0) then lineUps ls
                                 else [h : t | i <- [0..length (snd l) - 1], h <- [(snd l)!!i], t <- lineUps ( (fst l - 1, drop (i+1) (snd l)) : ls ), not (h `elem` t)]

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]

