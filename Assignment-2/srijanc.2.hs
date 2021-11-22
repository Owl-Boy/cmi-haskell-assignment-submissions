-- Question 1
import Data.List
shrink :: Eq a => [a] -> [a]
shrink [] = []
shrink (x:[])= [x]
shrink (x:y:xs)
   | x==y = shrink (y:xs) --where y= head xs
   | otherwise = x:shrink (y:xs)

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f []=[]
shrink' f [x]=[x]
shrink' f (x:y:xs)
   |f x == f y =shrink' f (x:xs)
   | otherwise =x:shrink' f (y:xs)


myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (a,b) 0 = [(a,b)]
knightMove (a,b) 1 =  sort [(x,y)|(x,y) <- squares,((a-x)*(b-y)==2||(a-x)*(b-y)==(-2))]
knightMove (a,b) n = shrink $ sort $concat $ map  knv (knightMove (a,b) (n-1))
     where 
     knv :: (Int,Int) -> [(Int,Int)]
     knv (a,b) =[(x,y)|(x,y) <- squares,((a-x)*(b-y)==2||(a-x)*(b-y)==(-2))]
-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x,y) 0 = [(x,y)]
knightMove' (x,y) n = shrink $ sort ( (knightMove (x,y) n) ++ (knightMove' (x,y) (n-1)))

-- Question 5

tuples :: [[a]] -> [[a]]
tuples []=[[]]
tuples (x:xs)=[[a]++b|a<-x,b<-tuples xs]


-- Question 6

injTuples :: Eq a => [[a]] -> [[a]]
injTuples []=[[]]
injTuples (x:xs)=[[a]++b|a<-x, b<-injTuples xs, not(a `elem` b)]
         


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
lineUps [ ] = [[]]
lineUps (x:xs) = [a ++ b | a<- subsetOfSize (fst x) (snd x) , b <- lineUps xs, unique a b]
   where 
   subsetOfSize ::Eq a =>  Int -> [a] -> [[a]]
   subsetOfSize 0 _ = [[]]
   subsetOfSize _ [] = []
   subsetOfSize n (x:xs) = [x : subs | subs <- subsetOfSize (n-1) xs] ++ subsetOfSize n xs
         

   unique :: Eq a => [a] -> [a] -> Bool
   unique [] q = True
   unique (x:xs) q 
    | x `elem` q = False
    | otherwise = unique xs q


fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]

