import Data.List

-- Question 1

shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink [x] = [x]
shrink (x:xs) = if x == head xs then shrink xs else x : shrink xs

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' _ [] = []
shrink' _ [x] = [x]
shrink' f (x:xs) = if f x == f (head xs) then shrink' f (x : tail xs) else x : shrink' f xs

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

-- ` foldr (.) id ` simply threads a value through the composition of a list of functions
knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove start n = shrink $ sort $ foldr (.) id (replicate n $ concatMap posAfter1Move) [start] where 
  posAfter1Move (x,y) = filter (\(x,y) -> x >= 0 && y >= 0) [(x+2,y+1), (x+2,y-1), (x-2,y+1), (x-2,y-1), (x+1, y-2), (x+1,y+2), (x-1, y-2), (x-1,y+2)]

-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' start n = shrink $ sort $ concatMap (knightMove start) [0..n]

-- Question 5

tuples :: [[a]] -> [[a]]
tuples lsts = foldr (\l ls -> [x : xs | x <- l, xs <- ls]) [[]] lsts

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples = filter (\l -> (length . nub $ l) == length l) . tuples 

-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]


combinations k ns = filter ((k==).length) $ subsequences ns

lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps [] = [[]]
lineUps lst@((m,xs):ys) = filter (( (==) . sum . map fst $ lst) . length . nub) $ (++) <$> (combinations m xs) <*> lineUps ys

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
