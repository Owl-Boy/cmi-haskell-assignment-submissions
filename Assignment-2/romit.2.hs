import Data.List



-- Question 1


shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink (x:xs) = x: get x xs where
    get n [] = []
    get n (y:ys) = if n == y then get y ys else y:get y ys





-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f [] = []
shrink' f (x:xs) = x: get x xs where
    get n [] = []
    get n (y:ys) = if f n == f y then get y ys else y:get y ys



myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]



-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove u v = shrink $ sort $ knightMoveIn u v where
    knightMoveIn a 0 = [a]
    knightMoveIn (x, y) n = concat [knightMove (x1, y1) (n-1) |  (x1, y1) <- squares,
                                                    abs (x1 - x) == 2 && abs (y1 - y) == 1 
                                                    || abs (x1 - x) == 1 && abs (y1 - y) == 2]








-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' u v = shrink $ sort $ knightMoveIn u v where
    knightMoveIn a 0 = [a]
    knightMoveIn (x, y) n = concat [knightMove' (x1, y1) (n-1) |  (x1, y1) <- squares,
                                                    abs (x1 - x) == 2 && abs (y1 - y) == 1 
                                                    || abs (x1 - x) == 1 && abs (y1 - y) == 2]
                            ++ [(x1, y1) |  (x1, y1) <- squares,
                                                    abs (x1 - x) == 2 && abs (y1 - y) == 1 
                                                    || abs (x1 - x) == 1 && abs (y1 - y) == 2]








-- Question 5

tuples :: [[a]] -> [[a]]
tuples [] = [[]]
tuples (l:ls) = [b:a | b <- l, a <- tuples ls]




-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples [] = [[]]
injTuples (l:ls) = [b:a | b <- l, a <- injTuples ls, b `notElem` a]


 



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
lineUps ((x, y):as) = [a | l <- list1 x y, l' <- lineUps as, let a = l ++ l', length (nub a) == length a]



list1 :: Eq a => Int -> [a] -> [[a]]
list1 0 l = [[]]
list1 n l = [snd a:snd rest | a <- zip [0..] l, rest <- zip [0..] (list1 (n-1) [snd b | b <- zip [0..] l, fst a < fst b])]


lineUps' :: Eq a => [(Int, [a])] -> [[a]]
lineUps' [] = [[]]
lineUps' ((x, y):as) = [a | l <- list2 x y, l' <- lineUps as, let a = l ++ l', length (nub a) == length a]


list2:: Eq a => Int -> [a] -> [[a]]
list2 0 l = [[]]
list2 n l = [(l!!a):rest | a <- [0..(length l - 1)], rest <- list1 (n-1) (drop a l)]





fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
