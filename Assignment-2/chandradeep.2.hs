import Data.List (nub, sort, sortOn)

-- Question 1

shrink :: Eq a => [a] -> [a]
shrink = shrink' id

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f = foldr (checkHead f) []
  where
    checkHead :: Eq b => (a -> b) -> a -> [a] -> [a]
    checkHead f x [] = [x]
    checkHead f x xs =
      if f (head xs) == f x
        then x : tail xs
        else x : xs

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0 ..]

-- Question 3

squares :: [(Int, Int)]
squares = [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]

knightFromTo :: [(Int, Int)] -> [(Int, Int)]
knightFromTo from = filter (`elem` squares) (concatMap possible from)
  where
    possible :: (Int, Int) -> [(Int, Int)]
    possible (x, y) =
      [ (x -2, y -1),
        (x -2, y + 1),
        (x -1, y -2),
        (x -1, y + 2),
        (x + 1, y -2),
        (x + 1, y + 2),
        (x + 2, y -1),
        (x + 2, y + 1)
      ]

knightMove :: (Int, Int) -> Int -> [(Int, Int)]
knightMove pos moves = shrink (sort (realKnightMove [pos] moves))
  where
    realKnightMove :: [(Int, Int)] -> Int -> [(Int, Int)]
    realKnightMove poss 0 = poss
    realKnightMove poss moves = realKnightMove (knightFromTo poss) (moves - 1)

-- Question 4

knightMove' :: (Int, Int) -> Int -> [(Int, Int)]
knightMove' pos moves = shrink (sort (realKnightMove' [pos] moves))
  where
    realKnightMove' :: [(Int, Int)] -> Int -> [(Int, Int)]
    realKnightMove' poss 0 = poss
    realKnightMove' poss moves =
      poss
        ++ realKnightMove'
          (knightFromTo poss)
          (moves - 1)

-- Question 5

tuples :: [[a]] -> [[a]]
tuples [] = []
tuples (xs : xss)
  | null xss = map (: []) xs
  | otherwise = concatMap (\x -> map (x :) (tuples xss)) xs

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples l = filter valid (tuples l)
  where
    valid :: Eq a => [a] -> Bool
    valid xs = nub xs == xs

-- Question 7

pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

subsets :: Eq a => (Int, [a]) -> [[a]]
subsets (0, _) = [[]]
subsets (_, []) = []
subsets (n, lst) =
  map (head lst :) (subsets (n -1, tail lst)) ++ subsets (n, tail lst)

-- injTuples (map subsets l) creates tuples of the form [[1,2,3],[1,2]], which
-- is not good enough for the validity checking, we need to transform that into
-- [1,2,3,1,2] and pass it to the validity checker again. So just using tuples
-- (map subsets l) is fine here.
--
-- The problem with directly using injTuples (no subsets) was that it solved
-- [1,2,3,1,5] from ever occurring, but not [1,2,3,4,5] followed by [1,2,3,5,4]
-- somewhere
--
-- Perhaps there was a better way to do this using injTuples directly.
lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps l = filter valid [concat ls | ls <- tuples (map subsets l)]
  where
    valid :: Eq a => [a] -> Bool
    valid xs = nub xs == xs

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
