import Data.List ( sort, sortOn )
-- Question 1

shrink :: Eq a => [a] -> [a]
shrink []     = []
shrink (x:xs) = x : shrink (dropWhile (== x) xs)

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' _ [] = []
shrink' _ [x] = [x]
shrink' f (x:xs@(y:ys))
                | f x == f y = shrink' f (x:ys)
                | otherwise = x : shrink' f xs

{-
Another Approach but neater :-

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f []         = []
shrink' f [x]        = [x]
shrink' f (x1:x2:xs) = [x1 | f x1 /= f x2] ++ f (x2:xs)

-}


myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]
-- move from each of the positions of previous turn then concat them then sort and remove the common positions
knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x,y) 0 = [(x,y)]
knightMove (x,y) 1 = sort $ moving (x,y)
knightMove (x,y) n = shrink' id $ sort $ concatMap moving (knightMove (x,y) (n-1))

-- In one turn number of possible positions of knight
moving :: (Int, Int ) -> [(Int , Int )]
moving (m,n) = filter (`elem` squares ) [(m+a,n+b) | (a,b) <- [(1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1)]]

-- Question 4 
-- number of possible positions at exactnly nth turn UNION all possible positions upto (n-1) turn then sort and shrink
knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x,y) 0 = [(x,y)]
-- knightMove' (x,y) 1 = shrink' id $ sort (knightMove (x,y) 1 ++ knightMove (x,y) 0) [No need for this line]
knightMove' (x,y) n = shrink' id $ sort (knightMove (x,y) n ++ knightMove' (x,y) (n-1))

-- Question 5

tuples :: [[a]] -> [[a]]
tuples [] = []
tuples [x] = [x]
tuples (l:ls)
 --       | null $ concat (l:ls) = [[]]  I already checked if lists are empty in fstlistmerge and listmerge
        | length (l:ls) == 2 = fstlistmerge (l:ls)
        | otherwise = listmerge l (tuples ls)

{- Tuples another approach

tuples :: [[a]] -> [[a]]
tuples = helper [[]]
  where
    helper :: [[a]] -> [[a]] -> [[a]]
    helper ls [] = ls
    helper ls ls2 = helper [x ++ [y] | x <- ls, y <- head ls2] (tail ls2)

-}

-- At first we have to make tuples from two lists in a list of lists to create 
fstlistmerge :: [[a]] -> [[a]]
--fstlistmerge [[],[]] = [[]] Works even without this case it just does nothing
fstlistmerge [x,y] = [[a,b] | a <- x, b <- y]

--after that we have a new list and with that make twuples with the list of lists made by list elements before that
listmerge :: [a] -> [[a]] -> [[a]]
--listmerge [] [[]] = [] Works even without this case it just does nothing
listmerge (x:xs) (y:ys) = [a : b | a <- x:xs, b <- y:ys]

-- Question 6
--make the tuples of list then just filter out the lists which has same element multiple times
injTuples :: Eq a => [[a]] -> [[a]]
injTuples [] = []
injTuples (l:ls) = filter distelem (tuples (l:ls))

--checks if the list has any element multiple times
distelem :: Eq a => [a] -> Bool
distelem [] = True
distelem (l:ls) = (l:ls) == compress (l:ls)

--remove all common elements
compress :: Eq a => [a] -> [a]
compress [] = []
compress (x:xs) = x : compress [a | a <- xs, a /= x]

-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]
-- first take last two tuple use a list of list of strings then just merge the new one with the previous one
lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps [] = []
lineUps (x:xs) =  filter distelem (listmerge' x (lineUps xs))

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]


--make all possible subset from list that is a list of lists
subset :: [a] -> [[a]]
subset [] = [[]]
subset (x:xs) = map (x:) (subset xs) ++ subset xs

--take only the subsets which have desired length
subset' :: (Int,[a]) -> [[a]]
subset' (_,[]) = [[]]
subset' (n,x:xs) = [l | l <- subset (x:xs), length l == n]

--from the desired subsets make a tuple of strings from two of them
fstlistmerge' :: Eq a => [(Int,[a])] -> [[a]]
fstlistmerge' [] = []
fstlistmerge' [(x,y)] = subset' (x,y)
fstlistmerge' [x,y] = filter distelem ([a ++ b | a <- subset' x, b <- subset' y])

--add a list of subset of strings to the derived list of strings from previous lists
listmerge' :: Eq a => (Int,[a]) -> [[a]] -> [[a]]
listmerge' (_,[]) [] = []
listmerge' (_,[]) (x:xs) = x:xs
listmerge' (x,y) [] = subset' (x,y)
listmerge' (x,y) (z:zs) = [a ++ b | a <- subset' (x,y), b <- z:zs]