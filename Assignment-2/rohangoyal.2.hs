import Graphics.Win32.GDI.Font (sYSTEM_FONT, sYMBOL_CHARSET)
import Graphics.Win32 (sYSTEM_FONT)
import Data.List (sortOn, sort)

-- Question 1

shrink :: Ord a => [a] -> [a]
shrinkHelp :: Ord a => [a] -> [a] -> [a]
shrinkHelp t [] = t
shrinkHelp [] (x:xs) = shrinkHelp [x] xs
shrinkHelp (x:xs) (y:ys)
    |x==y = shrinkHelp (x:xs) ys
    |otherwise = shrinkHelp (y:x:xs) ys
shrink x = reverse (shrinkHelp [] x)


--Question 2

shrink' :: Eq b => (a -> b) -> [a] -> [a]
shrink'Help :: Eq b => (a -> b) -> [a] -> [a] -> [a]
shrink'Help f t [] = t
shrink'Help f [] (x:xs) = shrink'Help f [x] xs
shrink'Help f (x:xs) (y:ys)
    |f x==f y = shrink'Help f (x:xs) ys
    |otherwise = shrink'Help f (y:x:xs) ys
shrink' f x = reverse (shrink'Help f [] x)
myNub :: Ord a => [a] -> [a]
myNub = map snd . sortOn fst . shrink' snd . sortOn snd . zip [0..]


--Question 3
positions :: (Int, Int)->[(Int, Int)]
positions (x,y)=[(x+2,y-1),(x+2,y+1),(x+1,y+2),(x-1,y-2),(x-2,y-1),(x-2,y+1),(x-1,y+2),(x+1,y-2)]
access_pos :: (Int, Int)->[(Int,Int)]
access_pos x = [(a,b)|(a,b)<-positions x, -1<a, a<8, -1<b, b<8]

access_pos' :: [(Int,Int)]->[[(Int,Int)]]
access_pos' = map access_pos

baa :: [[(Int,Int)]]->[(Int,Int)]->[(Int,Int)]
baa xs y = foldl (flip (++)) y xs

baa' :: [[(Int,Int)]]->[(Int,Int)]
baa' x = baa x []

smol :: [(Int,Int)]->[(Int,Int)]
smol = shrink . sort . baa' . access_pos'

squares' :: [(Int, Int)]->Int->[(Int, Int)]
squares' x 0 = x
squares' x n = squares' (smol x) (n-1)

squares'' :: [(Int, Int)]->Int->[(Int, Int)]
squares'' x 0 = x
squares'' x n = (shrink . sort) (x ++ squares'' (smol x) (n-1))

knightMove :: (Int, Int)->Int->[(Int, Int)]
knightMove x = squares' [x]

knightMove' :: (Int, Int)->Int->[(Int, Int)]
knightMove' x = squares'' [x]


f_tuple :: [a] -> [[a]]-> [[a]] -> [[a]]
f_tuple [] y z = z
f_tuple (x:xs) y z = f_tuple xs y (z++[x:a|a<-y])



tuples :: [[a]] -> [[a]]
tuples [] = []
tuples [a]=[[x]|x<-a]
tuples (x:xs) = f_tuple x (tuples xs) []

f_tuple' :: Eq a => [a] -> [[a]]-> [[a]] -> [[a]]
f_tuple' [] y z = z
f_tuple' (x:xs) y z = f_tuple' xs y (z++[x:a|a<-y, not(x `elem` a)])

injTuples :: Eq a => [[a]] -> [[a]]
injTuples [] = []
injTuples [a]=[[x]|x<-a]
injTuples (x:xs) = f_tuple' x (injTuples xs) []

disjoint :: Eq a => [a]-> [a] -> Bool 
disjoint [] b = True 
disjoint (x:xs) b = and [not(x `elem` b),(disjoint xs b)]

lists2 :: Eq a => [[a]]->[[a]]->[[a]]
lists2 [] _ = []
lists2 _ [] = []
lists2 (x:xs) y = [x++a|a<-y, disjoint x a]++ (lists2 xs y)

positionwise :: Eq a => (Int,[a])->[[a]]
positionwise (_,[]) = [] 
positionwise (0,_)=[]
positionwise (1,x) = [[a]|a<-x]
positionwise (n,x:xs) = ([x:a|a<-(positionwise (n-1,xs))])++(positionwise (n,xs))

compiling :: Eq a => [[[a]]] -> [[a]]
compiling [] = []
compiling [a] = a
compiling (x:xs) = lists2 x (compiling xs) 

lineUps :: Eq a => [(Int, [a])] -> [[a]]
lineUps = compiling . (map positionwise)