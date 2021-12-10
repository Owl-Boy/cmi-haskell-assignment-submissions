-- Submitted by Ayan Nath 
-- Roll No. BMC202121

import Data.List

-- Question 1

shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink [x] = [x]
shrink (x:y:xs)
  | x==y = shrink(y:xs)
  | otherwise = x:shrink(y:xs)

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f [] = []
shrink' f [x] = [x]
shrink' f (x:y:xs) 
  | f x == f y = shrink' f (x:xs)
  | otherwise = x:shrink' f (y:xs)

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x,y) 0 = [(x,y)]
knightMove (a,b) n = shrink $ sort $ intersect squares [t | (x,y) <- knightMove (a,b) (n-1), t <- [(x+1,y+2),(x+2,y+1),(x+1,y-2),(x+2,y-1),(x-2,y+1),(x-1,y+2),(x-2,y-1),(x-1,y-2)]]

-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x,y) 0 = [(x,y)]
knightMove' (a,b) n = shrink $ sort $ knightMove' (a,b) (n-1) ++ prevList where 
    prevList = intersect squares [t | (x,y) <- knightMove' (a,b) (n-1), t <- [(x+1,y+2),(x+2,y+1),(x+1,y-2),(x+2,y-1),(x-2,y+1),(x-1,y+2),(x-2,y-1),(x-1,y-2)]]

-- Question 5

tuples :: [[a]] -> [[a]]
tuples arg 
  | null arg = []
  | True `elem` [ null t | t <- arg ] = []
tuples [l] = [[x] | x <- l]
tuples (l:ls) = [ x:l1 | x <- l, l1 <- tuples ls]

-- Here, filtering dulplicacies is not feasible as a isn't given to be in Eq typeclass.

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples arg 
  | null arg = []
  | True `elem` [ null t | t <- arg ] = []
injTuples l = filter (\x -> x == nub x) (tuples l)

-- If distinct tuples are asked then replace the above line with
-- injTuples l = nub $ filter (\x -> x == nub x) (tuples l)

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
lineUps arg 
    | (arg == []) || (True `elem` [ n > length xs | (n,xs) <- arg ]) = [] 
    | otherwise = filter noDup (lineUpsDup arg) where 
        noDup :: Eq a => [a] -> Bool
        noDup x = nub x == x

        lineUpsDup :: Eq a => [(Int, [a])] -> [[a]]
        lineUpsDup [(0,l)] = [[]]
        lineUpsDup [(1,l)] = [[x] | x <- l]
        lineUpsDup [(m,l)] 
            | m == length l = [l]
            | otherwise = map (x:) (lineUpsDup [(m-1,xs)]) ++ lineUpsDup [(m,xs)] 
          where x:xs = l
        lineUpsDup ((m,l):ts) = [ x ++ y | x <- lineUpsDup [(m,l)], y <- lineUpsDup ts]


fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
