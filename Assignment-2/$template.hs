-- Question 1

shrink :: Eq a => [a] -> [a]
shrink = undefined

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' = undefined 

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove = undefined 

-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' = undefined 

-- Question 5

tuples :: [[a]] -> [[a]]
tuples = undefined 

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples = undefined 

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
lineUps = undefined 

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]

