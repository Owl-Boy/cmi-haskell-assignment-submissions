import Data.List
import System.IO

-- Question 1

ab ::Eq a => [a] -> [a]
ab (x:xs) 
  | xs == []       = [x]
  | x == head (xs) = [] 
  | otherwise      = [x]

shrink :: Ord a => [a] -> [a]
shrink l 
    | l ==[] = []
    | otherwise = ab (l) ++ shrink (tail (l))

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' f []     = []
shrink' f [x]    = [x] 
shrink' f (x: xs) = if f x == f (head xs)
                     then shrink' f (x : tail xs)
                     else x : shrink' f (xs)   

-- myNub :: Ord a => [a] -> [a]
-- myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

-- Question 3

squares :: [(Int,Int)]
squares = [(x,y) | x <- [0..7], y <- [0..7]]


lexiCo ::   [(Int, Int)] -> [(Int, Int)]
lexiCo [] = []
lexiCo (x:xs) = lexiCo smaller ++ [x] ++ lexiCo larger where
   smaller = [ (a,b) | (a,b) <- xs , (a,b) < x ]
   larger  = [ (c,d) | (c,d) <- xs , (c,d) > x ]

km :: (Int,Int)  -> [(Int, Int)]
km (x,y)  = filter squares
       [(x+2,y-1),(x+2,y+1),(x-2,y-1),(x-2,y+1),(x+1,y-2),(x+1,y+2),(x-1,y-2),(x-1,y+2)] 
       where squares (x,y) = x `elem` [0..7] && y `elem` [0..7]
       
km2 :: (Int,Int) -> [(Int, Int)]
km2 (x,y) = lexiCo $ concat $ map km (km (x,y))
   
km3 :: (Int,Int) -> [(Int, Int)]
km3 (x,y) = lexiCo $ concat $ map km (km2 (x,y))
   
km4 :: (Int,Int) -> [(Int, Int)]
km4 (x,y) = lexiCo $ concat $ map km (km3 (x,y))
   
km5 :: (Int,Int) -> [(Int, Int)]
km5 (x,y) = lexiCo $ concat $ map km (km4 (x,y))
   
km6 :: (Int,Int) -> [(Int, Int)]
km6 (x,y) = lexiCo $ concat $ map km (km5 (x,y))
   
km7 :: (Int,Int) -> [(Int, Int)]
km7 (x,y) = lexiCo $ concat $ map km (km6 (x,y))
   
km8 :: (Int,Int) -> [(Int, Int)]
km8 (x,y) = lexiCo $ concat $ map km (km7 (x,y))
   
km9 :: (Int,Int) -> [(Int, Int)]
km9 (x,y) = lexiCo $ concat $ map km (km8 (x,y))
  
km10 :: (Int,Int) -> [(Int, Int)]
km10 (x,y) = lexiCo $ concat $ map km (km9 (x,y))
   
km11 :: (Int,Int) -> [(Int, Int)]
km11 (x,y) = lexiCo $ concat $ map km (km10 (x,y))
   


   
knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove (x,y) n
         | n <= 0             = []
         | n>1 && x>7 || y >7 = []
         | n>1 && x<0 || y <0 = []
         | n == 1             = km  (x,y)
         | n == 2             = km2 (x,y)      
         | n == 3             = km3 (x,y)
         | n == 4             = km4 (x,y)
         | n == 5             = km5 (x,y)
         | n == 6             = km6 (x,y)
         | n == 7             = km7 (x,y)
         | n == 8             = km8 (x,y)
         | n == 9             = km9 (x,y) 
         | n>9 && even n      = km8 (x,y)     
         | n>9 && odd n       = km9 (x,y)     

     

-- Question 4 

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' (x,y) n
      | n <= 0             = []
      | n>1 && x>7 || y >7 = []
      | n>1 && x<0 || y <0 = []
      | n == 1             = km  (x,y)
      | n == 2             = lexiCo ( km2 (x,y) ++ km (x,y) )      
      | n == 3             = lexiCo ( km3 (x,y) ++ km2 (x,y) )
      | n == 4             = lexiCo ( km4 (x,y) ++ km3 (x,y) )
      | n == 5             = lexiCo ( km5 (x,y) ++ km4 (x,y) )
      | n == 6             = lexiCo ( km6 (x,y) ++ km5 (x,y) )
      | n == 7             = lexiCo ( km7 (x,y) ++ km6 (x,y) )
      | n == 8             = lexiCo ( km8 (x,y) ++ km7 (x,y) )
      | n == 9             = lexiCo ( km9 (x,y) ++ km8 (x,y) )
      | n>9 && even n             = lexiCo ( km10 (x,y) ++ km9 (x,y) )
      | n>9 && odd n            = lexiCo ( km11 (x,y) ++ km10 (x,y) )


-- Question 5

tuples :: [[a]] -> [[a]]
tuples [l]        = [l]
tuples [x,y]      = [[a,b] | a <- x , b <-y ]
tuples (x:xs)     = [concat[[a],b] | a <- x , b <- tuples xs  ]

-- Question 6

inj :: Eq a => [a] -> Bool
inj []       = True
inj [a,b]    = a/=b 
inj [a,b,c]  = a /= b && b /= c && c /= a
inj (x:xs)  = not ( x `elem` xs ) && inj (xs)

injTuples :: Eq a => [[a]] -> [[a]]
injTuples []  = []
injTuples [l] = [l]
injTuples [(x :xs),(y :ys)] = [[m,n] | m <- (x : xs) , n <- (y :ys) , m /=n ]
injTuples (l :ls) = [[x] ++ t | x <- l , t <- injTuples ls , elem x t == False ]

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
lineUps [x]          = ns [x]
lineUps (x:xs)    = [a ++ b | a <- (lineUps [x]  ) , b <- (lineUps xs) , inj (a ++ b )  && length ( a ++ b ) == len (x:xs)]


fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]



addele :: a -> [[a]] -> [[a]]
addele a []       =  []
addele a (y:ys)   = ( a: y )  : addele a ys


took :: (Int , [a]) -> [[a]]
took (0 , _)         = [[]]
took (n , [])        = []
took (1 , (x:xs))    = [[x]] ++ took (1 ,xs) 
took (n , (x:xs) )   = addele x (took ((n-1), xs)) ++ took (n , xs) 

ns :: [(Int, [a])] -> [[a]]
ns []              = []  
ns [a]             = took a 
ns (x : xs)        = took x ++ ns xs

len :: [(Int, [a])] -> Int
len []      = 0
len [(x,l)] = x
len (x :xs) = fst x + len xs 


