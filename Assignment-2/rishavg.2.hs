import Data.List
-- Question 1

shrink :: Ord a => [a] -> [a]
shrink  = foldr aux [] 
 where
     aux :: Ord a => a -> [a] -> [a]
     aux b [] = [b]
     aux b (y:ys) = if b==y then (y:ys) else (b:y:ys)
-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f  l =  foldr (aux f) [] l
 where
     aux :: (Eq b) =>(a->b) -> a -> [a] -> [a]
     aux g b [] = [b]
     aux g b (y:ys) = if (g b) == (g y) then (y:ys) else (b:y:ys)
myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]
-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x,y) i =sort(myNub (iter [(x,y)] i))
 where
     iter :: [(Int,Int)] -> Int -> [(Int, Int)]
     iter l 0 = l
     iter lis i =  iter (filter (\temp -> temp `elem` squares) (aux lis)) (i-1)
     aux  ::  [(Int,Int)] ->  [(Int,Int)]
     aux l = foldr gen [] l
     gen :: (Int,Int) -> [(Int,Int)] ->  [(Int,Int)]
     gen (x,y) l = [((x+2),(y-1)),((x+1),(y-2)),((x-2),(y+1)),((x-2),(y-1)),((x-1),(y-2)),((x+1),(y+2)),((x-1),(y+2)),((x+2),(y+1))] ++ l

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x,y) i =sort(myNub ((x,y):(iter' [(x,y)] i)))
 where
     iter' :: [(Int,Int)] -> Int -> [(Int, Int)]
     iter' l 0 = l
     iter' lis i = let u =filter (\temp -> temp `elem` squares) (aux lis) in  u ++ (iter' u (i-1))
     aux  ::  [(Int,Int)] ->  [(Int,Int)]
     aux l = foldr gen [] l
     gen :: (Int,Int) -> [(Int,Int)] ->  [(Int,Int)]
     gen (x,y) l = [((x+2),(y-1)),((x+1),(y-2)),((x-2),(y+1)),((x-2),(y-1)),((x-1),(y-2)),((x+1),(y+2)),((x-1),(y+2)),((x+2),(y+1))] ++ l

-- Question 5

tuples :: [[a]] -> [[a]]
tuples = foldr aux [] 
 where
     aux :: [a] ->[[a]] -> [[a]]
     aux [] l = []
     aux x [] = [[u] | u <- x]
     aux (x:xs) l = (a1 x l) ++ (aux xs l)
     a1 :: a -> [[a]] -> [[a]]
     a1 x [] = []
     a1 x (y:ys) = (x:y):(a1 x ys)

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples l = filter nodup (tuples l)
 where
     dup (x:xs) = aux [] (x:xs)
     aux :: Eq a => [a] -> [a] ->  [a]
     aux l [] = []
     aux l (y:ys) = if y `elem` l then aux l ys else y:(aux (y:l) ys)
     nodup u = if dup u == u then True else False
-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

lineUps :: (Eq a) => [(Int, [a])] -> [[a]]
lineUps l = filter nodup (conv l)
 where
     conv :: [(Int,[a])] -> [[a]]
     conv l = map (foldr (++) [])  (tuples (map (\x -> choose (fst x) (snd x)) l ))
     choose :: Int -> [b] ->[[b]]
     choose 0 l =[[]]
     choose _ [] = []
     choose i (x:xs) =if (length (x:xs))>=i then (map (x:) (choose (i-1) xs)) ++ (choose i xs) else []
     dup (x:xs) = aux [] (x:xs)
     aux :: Eq a => [a] -> [a] ->  [a]
     aux l [] = []
     aux l (y:ys) = if y `elem` l then aux l ys else y:(aux (y:l) ys)
     nodup u = if dup u == u then True else False


fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
