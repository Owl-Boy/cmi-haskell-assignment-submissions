import Data.List

-- Question 1

shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink [x] = [x]
shrink (x : xs)
   | x == head xs      = shrink (x : (tail xs))
   | otherwise         = x : shrink xs
   
   

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f [] = []
shrink' f (x : xs)
   | length (x : xs) <= 1           = (x : xs)
   | f x == f (head xs)             = shrink' f (x : tail xs)
   | otherwise                      = x : shrink' f xs

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]




-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

--self defined start
knightMoveUnsort :: (Int,Int) -> Int -> [(Int,Int)]
knightMoveUnsort (x,y) n
   | n == 0    = [(x,y)]
   | n == 1    = [(x1,y1) | (x1,y1) <- squares, ((abs (x1-x) == 2 && abs (y1-y) == 1)||(abs (x1-x) == 1 && abs (y1-y) == 2))]
   | otherwise = rem (x,y) n where 
   rem (x,y) n = myNub (concat (map (`knightMoveUnsort` (n-1)) (knightMoveUnsort (x,y) 1)))
--self defined end

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x,y) n = sortOn fst (sortOn snd (knightMoveUnsort (x,y) n))



-- Question 4 

--self defined start
knightMove'Unsort :: (Int,Int) -> Int -> [(Int,Int)]
knightMove'Unsort (x,y) n
   | n == 0     = [(x,y)]
   | otherwise  = (knightMoveUnsort (x,y) n) ++ (knightMove'Unsort (x,y) (n-1))
--self defined end

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x,y) n = sortOn fst (sortOn snd (myNub (knightMove'Unsort (x,y) n)))





-- Question 5

--self defined start
myPrefix :: a -> [[a]] -> [[a]]
myPrefix a [] = []
myPrefix a (x:xs) = (a:x) : myPrefix a xs
--self defined end

tuples :: [[a]] -> [[a]]
tuples [] = []
tuples [x] = [[a] | a <- x ]
tuples ([]:xs) = []
tuples (x:xs) = (myPrefix (head x) (tuples xs)) ++ (tuples ((tail x):xs))






-- Question 6

--self defined begin
myListEqu :: Eq a => [a] -> [a] -> Bool
myListEqu [] [] = True
myListEqu [] _ = False
myListEqu _ [] = False
myListEqu (x:xs) (y:ys) = x==y && myListEqu xs ys
--self defined end

injTuples :: Eq a => [[a]] -> [[a]]
injTuples n = [l | l <- tuples n, myListEqu l (nub l)]









-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

--self defined begin
pairConvert :: (Int,[a]) -> [[a]]
pairConvert (0,l) = []
pairConvert (n,l) = l : pairConvert (n-1,l)

selfTuples :: Eq a => [a] -> [[a]]
selfTuples l = [n | n <- (injTuples (pairConvert ((length l) , l)))]

myListEqu' :: Eq a => [a] -> [a] -> Bool
myListEqu' [] [] = True
myListEqu' [] _ = False
myListEqu' _ [] = False
myListEqu' x l = (elem x (selfTuples l))

myListIne' :: Eq a => [a] -> [a] -> Bool
myListIne' a b = not (myListEqu' a b)

injTuples' :: Eq a => [[a]] -> [[a]]
injTuples' l = sieve (injTuples l) where
   sieve [] = []
   sieve (x:xs) = x : sieve [n | n <- xs, myListIne' n x] 

list' :: Eq a => [(Int, [a])] -> [[[a]]]
list' l = map pairConvert l

lineUps' :: Eq a => [[[a]]] -> [[[a]]]
lineUps' [] = []
lineUps' (x:xs) = [injTuples' x] ++ (lineUps' xs)

lineUps'' :: Eq a => [(Int,[a])] -> [[[a]]]
lineUps'' = injTuples' . lineUps' . list'

lineUpss :: Eq a => [[[a]]] -> [[a]]
lineUpss [] = []
lineUpss (x:xs) = [foldr (++) [] x] ++ lineUpss xs
--self defined end

lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps l = [l1 | l1 <- lineUpss (lineUps'' l), myListEqu l1 (nub l1)]

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]