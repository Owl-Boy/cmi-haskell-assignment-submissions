import Data.List
-- Question 1

shrink :: Eq a => [a] -> [a]
shrink [] =[]
shrink [x] = [x]
shrink (x:y:xs)
 | x /= y = x: shrink(y:xs)
 | otherwise = x : shrink(dropWhile (==x) xs)

-- Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink' _ [] =[]
shrink' _ [x] = [x]
shrink' f (x:y:xs)
 |f x /= f y = x: shrink' f (y:xs)
 | otherwise = x : shrink' f (dropWhile (\y->f y == f x)  xs)

myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]

knightMove :: (Int,Int) -> Int -> [(Int,Int)]
knightMove1 k 0 = [k]
knightMove1 (x,y) z = concat [a,b,c,d,e,f,g,h]
 where a = [(m+2,n+1) |(m,n)<-l, 0<=m && m<=5 && 0<=n && n<=6]
       b = [(m+2,n-1) |(m,n)<-l, 0<=m && m<=5 && 1<=n && n<=7]
       c = [(m-2,n+1) |(m,n)<-l, 2<=m && m<=7 && 0<=n && n<=6]
       d = [(m-2,n-1) |(m,n)<-l, 2<=m && m<=7 && 1<=n && n<=7]
       e = [(m+1,n+2) |(m,n)<-l, 0<=m && m<=6 && 0<=n && n<=5]
       f = [(m+1,n-2) |(m,n)<-l, 0<=m && m<=6 && 2<=n && n<=7]
       g = [(m-1,n+2) |(m,n)<-l, 1<=m && m<=7 && 0<=n && n<=5]
       h = [(m-1,n-2) |(m,n)<-l, 1<=m && m<=7 && 2<=n && n<=7]
       l  = shrink(sort(knightMove1 (x,y) (z-1)))
knightMove (x,y) z = shrink(sort (knightMove1 (x,y) z))

knightMove' :: (Int,Int) -> Int -> [(Int,Int)]
knightMove' x 0 = [x]
knightMove' (x,y) z =shrink ( sort ( knightMove (x,y) z ++ knightMove' (x,y) (z-1)))

-- Question 5
tuples :: [[a]] -> [[a]]
tuples [l]=[[x]|x<-l]
tuples (l:ls)=[x:y|x<-l,y<-tuples ls]

-- Question 6
injTuples :: Eq a => [[a]] -> [[a]]
injTuples l = filter g (tuples l)
 where g [x] = True
       g (x:xs)= notElem x xs && g xs

--Question 7
pg, sg, sf, pf, c :: [String]
pg = ["LeBron", "Russ", "Rondo", "Nunn"]
sg = ["Monk", "Baze", "Bradley", "Reaves", "THT", "Ellington", "LeBron"]
sf = ["LeBron", "Melo", "Baze", "Ariza"]
pf = ["LeBron", "AD", "Melo"]
c = ["AD", "DJ", "Dwight"]

traditionalLineUps :: [[String]]
traditionalLineUps = injTuples [pg, sg, sf, pf, c]

lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps [(1,y)] = [[a]|a<-y]
lineUps ((1,y):xs) =[a:b|a<-y,b<-lineUps xs]
lineUps ((x,y):xs) = filter g ([a:b|a<-y,b<-lineUps((x-1,removeItem a (dropWhile (/=a) y)):xs)])
 where g [x] = True
       g (x:xs)= notElem x xs && g xs
       removeItem _ []                 = []
       removeItem x (y:ys) | x == y    = removeItem x ys
                    | otherwise = y : removeItem x ys


fc1, bc1 :: [String]
fc1 = ["LeBron", "AD", "Dwight", "Melo", "THT"]
bc1 = ["LeBron", "Russ", "Baze"]

fc2, bc2 :: [String]
fc2 = ["LeBron", "Wade", "Bosh", "Haslem"]
bc2 = ["LeBron", "Wade", "Allen"]
