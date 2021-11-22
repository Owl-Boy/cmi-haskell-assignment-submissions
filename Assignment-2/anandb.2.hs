import Data.List
--1
shrinkt :: Eq a => [a] -> a -> [a]
shrinkt [] y = []
shrinkt (x:xs) y = if (x == y) then (shrinkt xs x) else [x] ++ (shrinkt xs x)

shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink l = [(head l)] ++ shrinkt l (head l)

tests_shrink :: [Bool]
tests_shrink = [
    shrink [1,2,3] == [1,2,3],
    shrink [1,1,1,2,2,3,3,3,3,4,4] == [1,2,3,4],
    shrink [1,2,2,3,3,4,3,2] == [1,2,3,4,3,2],
    shrink "aaabbbaaacccaaddaaa" == "abacada"
    ]

--2
shrinkt' :: Eq b => (a -> b) -> [a] -> a -> [a] 
shrinkt' f [] y = []
shrinkt' f (x:xs) y = if (f(x) == f(y)) then (shrinkt' f xs x) else [x] ++ (shrinkt' f xs x)

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f [] = []
shrink' f l = [(head l)] ++ shrinkt' f l (head l)

tests_shrink' :: [Bool]
tests_shrink' = [
    shrink' snd [(1,1),(2,2),(3,3)] == [(1,1),(2,2),(3,3)],
    shrink' snd (zip [0..] [1,1,1,2,2,3,3,3,3,4,4]) == [(0,1),(3,2),(5,3),(9,4)],
    shrink' fst (zip [10,9..] [1,2,2,3,3,4,3,2]) == [(10,1),(9,2),(8,2),(7,3),(6,3),(5,4),(4,3),(3,2)],
    shrink' id "aaabbbaaacccaaddaaa" == "abacada"
    ]

--3
isl :: (Int, Int) -> (Int, Int) -> Bool
isl x y = ((fst x - fst y)^2 + (snd x - snd y)^2 == 5)

squares = [(x,y)| x<-[0..7], y<-[0..7]]

knightMove :: (Int, Int) -> Int -> [(Int, Int)]
knightMove t 0 = [t]
knightMove t 1 = [s | s <- squares, isl s t == True]
knightMove t n = shrink(sort(concat(map knightm [s | s <- squares, isl s t == True]))) where knightm t = knightMove t (n-1)

tests_knightMove :: [Bool]
tests_knightMove = [
    knightMove (0,0) 0 == [(0,0)],
    knightMove (0,0) 1 == [(1,2),(2,1)],
    knightMove (0,0) 2 == [(0,0),(0,2),(0,4),(1,3),(2,0),(2,4),(3,1),(3,3),(4,0),(4,2)],
    knightMove (0,0) 3 == [(0,1),(0,3),(0,5),(1,0),(1,2),(1,4),(1,6),(2,1),(2,3),(2,5),(3,0),
    (3,2),(3,4),(3,6),(4,1),(4,3),(4,5),(5,0),(5,2),(5,4),(6,1),(6,3)],
    knightMove (2,2) 2 == [(0,2),(0,6),(1,1),(1,3),(1,5),(2,0),(2,2),(2,4),(2,6),(3,1),
    (3,3),(3,5),(4,2),(4,6),(5,1),(5,3),(5,5),(6,0),(6,2),(6,4)]
    ]

--4
knightMove' :: (Int, Int) -> Int -> [(Int, Int)]
knightMove' t 0 = [t]
knightMove' t 1 = [t] ++ [s | s <- squares, isl s t == True]
knightMove' t n = shrink(sort(concat(map knightm [s | s <- squares, isl s t == True]))) where knightm t = knightMove' t (n-1)

tests_knightMove' :: [Bool]
tests_knightMove' = [
    knightMove' (0,0) 0 == [(0,0)],
    knightMove' (0,0) 1 == [(0,0),(1,2),(2,1)],
    knightMove' (0,0) 2 == [(0,0),(0,2),(0,4),(1,2),(1,3),(2,0),(2,1),(2,4),(3,1),(3,3),(4,0),(4,2)],
    knightMove' (0,0) 3 == [(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(1,0),(1,2),(1,3),(1,4),(1,6),(2,0),
    (2,1),(2,3),(2,4),(2,5),(3,0),(3,1),(3,2),(3,3),(3,4),(3,6)
    ,(4,0),(4,1),(4,2),(4,3),(4,5)
    ,(5,0),(5,2),(5,4)
    ,(6,1),(6,3)],
    knightMove' (2,2) 2 == [(0,1),(0,2),(0,3),(0,6)
    ,(1,0),(1,1),(1,3),(1,4),(1,5)
    ,(2,0),(2,2),(2,4),(2,6)
    ,(3,0),(3,1),(3,3),(3,4),(3,5)
    ,(4,1),(4,2),(4,3),(4,6)
    ,(5,1),(5,3),(5,5)
    ,(6,0),(6,2),(6,4)]
    ]

--5
tuples :: [[a]] -> [[a]]
tuples [[]] = [[]]
tuples [l] = [[x]|x <- l]
tuples(l:ls) = [[x] ++ t | x <- l, t <- tuples(ls)]

tests_tuples :: [Bool]
tests_tuples = [
    tuples [[1,2],[3,4]] == [[1,3],[1,4],[2,3],[2,4]],
    tuples [[1,2,3],[3,4,5]] == [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5],[3,3],[3,4],[3,5]],
    tuples [[1,2],[3,4],[2,4]] == [[1,3,2],[1,3,4],[1,4,2],[1,4,4],[2,3,2],[2,3,4],[2,4,2],[2,4,4]]
    ]

--6
tform :: Eq a => [[a]] -> [a] -> [[a]]
tform ll l = concat [[r ++ [x] | x <- l, elem x r == False] | r <- ll]

injTuples :: Eq a => [[a]] -> [[a]]
injTuples [[]] = [[]]
injTuples [l] = [[x] |x <- l]
injTuples(l:ls) = foldl tform [[x] | x <- l] ls 

tests_injTuples :: [Bool]
tests_injTuples = [
    injTuples [[1,2],[3,4]] == [[1,3],[1,4],[2,3],[2,4]],
    injTuples [[1,2,3],[3,4,5]] == [[1,3],[1,4],[1,5],[2,3],[2,4],[2,5],[3,4],[3,5]],
    injTuples [[1,2],[3,4],[2,4]] == [[1,3,2],[1,3,4],[1,4,2],[2,3,4]]
    ]

--7
ind :: (Eq a, Eq b, Num b) => a -> [a] -> b -> b 
ind a b i
 | a == head b = i
 | otherwise = ind a (tail b) (i+1)

otform :: Eq a => [[a]] -> [a] -> [[a]]
otform ll l = concat [[ x ++ [y] | y <- snd (splitAt ((ind (x !! (length(x) - 1)) l 0) + 1) l), y `notElem` x] | x <- ll]

otformn :: (Eq a, Eq b, Num b) => [[a]] -> (b, [a])-> [[a]]
otformn ll (1, m) = tform ll m
otformn ll (n, m) = otform (otformn ll (n-1, m)) m

lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps (ml:l) = foldl otformn (otformn [[]] ml) l

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]

tests_lineUps :: [Bool]
tests_lineUps = [
    lineUps [(3,fc1), (2,bc1)] ==
    [["LeBron","AD","Dwight","Russ","Baze"]
    ,["LeBron","AD","Melo","Russ","Baze"]
    ,["LeBron","AD","THT","Russ","Baze"]
    ,["LeBron","Dwight","Melo","Russ","Baze"]
    ,["LeBron","Dwight","THT","Russ","Baze"]
    ,["LeBron","Melo","THT","Russ","Baze"]
    ,["AD","Dwight","Melo","LeBron","Russ"]
    ,["AD","Dwight","Melo","LeBron","Baze"]
    ,["AD","Dwight","Melo","Russ","Baze"]
    ,["AD","Dwight","THT","LeBron","Russ"]
    ,["AD","Dwight","THT","LeBron","Baze"]
    ,["AD","Dwight","THT","Russ","Baze"]
    ,["AD","Melo","THT","LeBron","Russ"]
    ,["AD","Melo","THT","LeBron","Baze"]
    ,["AD","Melo","THT","Russ","Baze"]
    ,["Dwight","Melo","THT","LeBron","Russ"]
    ,["Dwight","Melo","THT","LeBron","Baze"]
    ,["Dwight","Melo","THT","Russ","Baze"]],
    lineUps [(3,fc2), (2,bc2)] ==
    [["LeBron","Bosh","Haslem","Wade","Allen"]
    ,["Wade","Bosh","Haslem","LeBron","Allen"]]
    ]

verify = [tests_shrink, tests_shrink', tests_knightMove, tests_knightMove', tests_tuples, tests_injTuples, tests_lineUps]

--7* (Permutations, so can give jersey order XP)
dupln :: (Eq a, Eq b, Num b) => [a] -> b -> [[a]]
dupln l 0 = [[]]
dupln l 1 = [l]
dupln l n = [l] ++ dupln l (n-1)

tform' :: (Eq a, Eq b, Num b) => [[a]] -> (b, [a]) -> [[a]]
tform' l m = foldl tform l (dupln (snd m) (fst m))

lineUps' :: Eq a => [(Int, [a])] -> [[a]] 
lineUps' (ml:l) = foldl tform' (foldl tform [[x] | x <- (snd ml)] (dupln (snd ml) ((fst ml) - 1))) l

