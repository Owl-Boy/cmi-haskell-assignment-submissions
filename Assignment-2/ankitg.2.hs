import Data.List
-- problem 1
shrink :: Eq a => [a] ->[a]
shrink [] = []
shrink [x] = [x]
shrink (x:xs) | x==(head xs) = shrink xs
              | otherwise = x: shrink xs

--problem 2
shrink' :: Eq b =>(a ->b) ->[a] ->[a]
shrink' f [] = []
shrink' f [x] = [x]
shrink' f (x:xs) | f x == f (head xs) = shrink' f (x: tail xs)
                 | otherwise = x: shrink' f xs

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- problem 3
squares :: [(Int, Int)]
squares = [(x,y) | x <- [0..7], y<- [0..7]]
check :: (Int, Int) -> Bool
check (a,b) | (a,b) `elem` squares = True 
            | otherwise = False 

knightMove :: (Int, Int) -> Int -> [(Int, Int)]
knightMove (x,y) 0 = [(x,y)]
knightMove (x,y) 1 =sort( filter check [(x+2,y+1),(x+1,y+2),(x-2,y+1),(x-1,y+2),(x-2,y-1),(x-1,y-2),(x+2,y-1),(x+1,y-2)])
knightMove (x,y) n = shrink ( sort(concat ( map func (knightMove (x,y) (n-1))))) where
    func (x,y)= knightMove (x,y) 1

--problem 4
knightMove' :: (Int, Int) -> Int -> [(Int, Int)]
knightMove' (x,y) n = shrink( sort(concat [knightMove (x,y) n| n<-[0..n]]))

-- problem 5
tuples :: [[a]] -> [[a]]
tuples (x:xs) | length( x:xs)>1 = [y:z| y<-x,z<- tuples xs]
              | otherwise = [[y]| y<- x]

-- problem 6
dupcheck :: Eq a => [a] -> Bool 
dupcheck [] = False
dupcheck (x:xs) | x `notElem` xs = dupcheck xs
                | otherwise = True
injTuples :: Eq a => [[a]] -> [[a]] 
injTuples (x:xs) = [ y | y<- tuples (x:xs) , not (dupcheck y)]

-- problem 7
pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

choose :: [a] ->Int -> [[a]]
choose _ 0 =[[]]
choose [] _ = []
choose (x:xs) n = (map (\ys -> x:ys )) (choose xs (n-1)) ++ (choose xs n)

lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps (x:xs) | length(x:xs)>1 = [y ++ z | y<-choose (snd x) (fst x), z<-lineUps xs, not(dupcheck (y ++ z))]
               | otherwise = choose (snd x) (fst x)

fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]

--g1,g2 :: [String]
--g1 = ["a","b","c","d"]
--g2 = [ "e", "f", "g"]
ank :: Int

ank =minimum [i| i<- [1..10] , knightMove'(0,6) i == squares]