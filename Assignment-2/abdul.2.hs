import Data.List
import Control.Applicative

-- Question 1

shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink [x] = [x]
shrink (x:xs)|(x == head xs)=shrink xs|1>0=x:shrink xs

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
knightMove start n = myNub . foldr (.) id (replicate n (>>=posAfter1Move)) $ [start] where 
  posAfter1Move (x,y) = filter (liftA2 (&&) ((>= 0) . fst) ((>= 0) . snd)) [(x+2,y+1), (x+2,y-1), (x-2,y+1), (x-2,y-1), (x+1, y-2), (x+1,y+2), (x-1, y-2), (x-1,y+2)]

-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' = (myNub.) . (. enumFromTo 0) . concatMap . knightMove

-- Question 5

tuples :: [[a]] -> [[a]]
tuples = mapM id

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples = filter (liftA2 (==) length (length . nub)) . tuples 

-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

combinations = (. subsequences) . filter . (. length) . (==)
lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps = filter (liftA2 (==) length (length . nub)) . foldr (<*>) [[]] . map (((++) <$>) . uncurry combinations)

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
