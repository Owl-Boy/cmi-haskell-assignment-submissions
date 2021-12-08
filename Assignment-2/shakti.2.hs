import Data.List ()
import System.IO ()
 
--problem 1 
shrink :: Eq a => [a] -> [a]
shrink = shrink' id

--problem 2
shrink' :: Eq b => (a -> b) -> [a] ->[a]
shrink' f [] = []
shrink' f [x] = [x]
shrink' f (x:xs) = if f x == f (head xs) then shrink' f (x:tail xs) else x:shrink' f xs

--problem 3
valid :: [(Int,Int)] -> [(Int,Int)] 
valid l = [(x,y) | (x,y) <- l , x >= 0 && x <= 7 , y >= 0 && y <= 7 ]

sort' :: Ord a => [a] -> [a] 
sort' [] = []
sort' (x:xs) = sort' lesser  ++ [x] ++ sort' greater  where
           lesser  = [y | y <- xs, y <= x]
           greater  = [y | y <- xs, y > x]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x,y) n
           | x < 0 || y < 0 = error "bad request"
           | x > 7 || y > 7 = error "bad request"
           | n < 0          = error "bad request"
knightMove (x,y) 0 = [(x,y)]
knightMove (x,y) 1 = valid [(x-2,y-1),(x-2,y+1),(x-1,y-2),(x-1,y+2),(x+1,y-2),(x+1,y+2),(x+2,y-1),(x+2,y+1)]
knightMove (x,y) n = shrink (sort' (valid l)) where 
                    l = [m | z <- valid (knightMove (x,y) (n-1)), m <- knightMove z 1]


--problem 4
knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x,y) n
            
           | x < 0 || y < 0 = error "bad request"
           | x > 7 || y > 7 = error "bad request"
           | n < 0          = error "bad request"

knightMove' (x,y) 0 = [(x,y)]
knightMove' (x,y) 1 = sort' (knightMove (x,y) 1 ++ knightMove' (x,y) 0)
knightMove' (x,y) n = shrink (sort' (knightMove (x,y) n ++ knightMove' (x,y) (n-1)))



--problem 5
tuples :: [[a]] -> [[a]]
tuples [] = []
tuples [l] = [l]
tuples [(x:xs),(y:ys)] = [[m,n] | m <- (x:xs), n <- (y:ys) ]
tuples (l:ls) = [[x] ++ t | x <- l, t <- tuples ls]


--problem 6
injTuples :: (Ord a,Eq a) => [[a]] -> [[a]]
injTuples [] = []
injTuples [l] = [l]
injTuples [l,r] = [[m,n] | m <- l, n <- r, m /= n ]
injTuples (l:ls) =  [t | t <- tuples (l:ls), shrink (sort' t) == sort' t] 


-- problem 7
pg = ["LeBron","Russ","Rondo","Nunn"]
sg = ["Monk","Baze","Bradley","Reaves","THT","Ellington","LeBron"]
sf = ["LeBron","Melo","Baze","Ariza"]
pf = ["LeBron","AD","Melo"]
c  = ["AD","DJ","Dwight"]

traditionalLineUps :: [[String]] 
traditionalLineUps = injTuples [pg,sg,sf,pf,c]


z :: [(Int,[a])] -> [[a]]
z [(0,[])]    = []
z [(n,_)]     
     | n < 0 = error "its illegal"
z [(1,(x:xs))]     = [(x:xs)]
z [(2,(x:xs))]     = [(x:xs),(x:xs)]
z [(n,(x:xs))]     = [(x:xs)] ++ z [(n-1,(x:xs))] 

z1 :: (Ord a, Eq a) => [(Int,[a])] -> [[a]]
z1 = injTuples . z

z2 ::  (Ord a, Eq a) => [(Int,[a])] -> [[a]]
z2 = map sort' . z1

z3 :: (Ord a, Eq a) => [(Int,[a])] -> [[a]]
z3 = sort' . z2 

z4 :: (Ord a, Eq a) => [(Int,[a])] -> [[a]]
z4 = shrink . z3 

z5 :: (Ord a, Eq a) => [(Int,[a])] -> [[[a]]]
z5 [] = []
z5 [(n,(x:xs))] = [z4 [(n,(x:xs))]]
z5 (l:ls)       = [z4 [l]] ++  z5 ls

z6 l = injTuples (z5 l)

z7 l = map concat (z6 l)

z8 l = map sort' (z7 l)

aux :: (Ord a, Eq a) => [[a]] -> [[a]]
aux [] = []
aux [x] = if length (shrink (sort' x)) == length x then [x] else [] 
aux (x:xs) = if length (shrink (sort' x)) == length x then x:aux xs else aux xs

lineUps :: (Ord a, Eq a) => [(Int,[a])] -> [[a]]
lineUps [] = []
lineUps l = aux (z8 l)


 
fc1 :: [String]
fc1 = ["LeBron","AD","Dwight","Melo","THT"]
bc1 :: [String]
bc1 = ["LeBron","Russ","Baze"]
fc2 :: [String] 
fc2 = ["LeBron","Wade","Bosh","Haslem"]
bc2 :: [String]
bc2 = ["LeBron","Wade","Allen"]
