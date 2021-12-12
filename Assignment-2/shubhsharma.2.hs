import Data.List

-- support function

posValue :: (Int, Int) -> Int
posValue (a, b) = 8 * a + b

in1 :: Eq a => a -> [a] -> Bool
in1 elem ls
  | null ls = False
  | otherwise = elem == head ls || in1 elem (tail ls)

-- Question 1

shrink :: Eq a => [a] -> [a]
shrink ls
  | null ls = []
  | length ls == 1 = ls
  -- the head of the list will always be in the list after applying shrink
  | otherwise = shrink (init ls) ++ [last ls | last ls /= ls !! (length ls - 2)]

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' func ls
  | null ls = []
  | length ls == 1 = ls
  | otherwise = shrink' func (init ls) ++ [last ls | func (last ls) /= func (ls !! (length ls - 2))]

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0 ..]

-- Question 3

squares :: [(Int, Int)]
squares = [(x, y) | x <- [0 .. 7], y <- [0 .. 7]]

knightMove :: (Int, Int) -> Int -> [(Int, Int)]
knightMove pos = helper [pos]
  where
    helper :: [(Int, Int)] -> Int -> [(Int, Int)]
    helper pos steps
      | steps == 0 = pos
      | otherwise = helper (moveList pos) (steps - 1)

    moveList :: [(Int, Int)] -> [(Int, Int)]
    -- takes a list of positions and finds where the knight can go after one move from positions in the list
    moveList = shrink . sortOn posValue . concatMap moveTo

    moveTo :: (Int, Int) -> [(Int, Int)]
    -- gives a list of positions that the knight can go to from a given position
    moveTo currentPos =
      [ pos | pos <- squares, (abs (fst pos - fst currentPos) == 1 && abs (snd pos - snd currentPos) == 2)
                                || (abs (fst pos - fst currentPos) == 2 && abs (snd pos - snd currentPos) == 1)
      ]

-- Question 4

knightMove' :: (Int, Int) -> Int -> [(Int, Int)]
knightMove' pos steps
  | steps == 1 = knightMove pos steps
  | otherwise = (shrink . sortOn posValue) (knightMove pos steps ++ knightMove' pos (steps - 1))

-- Question 5

tuples :: [[a]] -> [[a]]
tuples = helper [[]]
  where
    helper :: [[a]] -> [[a]] -> [[a]]
    helper ls [] = ls
    helper ls ls2 = helper [x ++ [y] | x <- ls, y <- head ls2] (tail ls2)

-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples = helper [[]]
  where
    helper :: Eq a => [[a]] -> [[a]] -> [[a]]
    helper ls [] = ls
    helper ls ls2 = helper [x ++ [y] | x <- ls, y <- head ls2, not (y `in1` x)] (tail ls2)

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
lineUps = injTuples2 . posSets
  where
    posSets :: Eq a => [(Int, [a])] -> [[[a]]]
    -- make possible sets for each position which is later combined
    posSets = map (removeReps . injTuples . posTuples)
      where
        posTuples :: Eq a => (Int, [a]) -> [[a]]
        -- makes copies of n copies of a so that injTuples will select n elements from it
        posTuples (0, _) = []
        posTuples (n, a) = a : posTuples (n - 1, a)

        removeReps :: Eq a => [[a]] -> [[a]]
        -- removes permutations of a lineup
        removeReps [] = []
        removeReps ls = removeReps (init ls) ++ [last ls | not (last ls `in2` init ls)]

        in2 :: Eq a => [a] -> [[a]] -> Bool
        in2 _ [] = False
        in2 elem (x : xs) = elem `subset` x || in2 elem xs

        subset :: Eq a => [a] -> [a] -> Bool
        -- checks if 2 elements are equal (because if length is same then check for subset is the same as checking for equality)
        subset [] _ = True
        subset (x : xs) ls2 = x `in1` ls2 && subset xs ls2

    injTuples2 :: Eq a => [[[a]]] -> [[a]]
    -- combines all sets for each position if there are no common members
    injTuples2 = helper [[]]
      where
        helper :: Eq a => [[a]] -> [[[a]]] -> [[a]]
        helper ls1 [] = ls1
        helper ls1 (x : xs) = helper [m ++ n | m <- ls1, n <- x, m `disjoint` n] xs
          where
            disjoint :: Eq a => [a] -> [a] -> Bool
            disjoint ls1 ls2 = null ([x | x <- ls1, y <- ls2, x == y])

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
