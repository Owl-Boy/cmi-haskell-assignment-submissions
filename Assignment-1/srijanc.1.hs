--Problem1
nextSquare :: Integer -> Integer
nextSquare n=nsq 0 where
   nsq :: Integer -> Integer
   nsq i
      | i*i>=n =i*i
	  |otherwise = nsq (i+1)
	  
--Problem2
previousCube :: Integer -> Integer
previousCube n
            | n<0 = pc (-1)
	        | otherwise = ncb 0 
			 where 
		       ncb :: Integer -> Integer
		       ncb i
			      | i*i*i>n = (i-1)*(i-1)*(i-1)
				  | otherwise = ncb (i+1)
				  
		       pc :: Integer -> Integer
		       pc i
			     | i*i*i <= n =i*i*i
				 | otherwise = pc (i-1)
				 
--Problem3
nextPalin :: Integer -> Integer
nextPalin n
         | n<=0      = 0
         | ispal n == True  = n
		 | otherwise = nextPalin (n+1)
		  
		 where
		  ispal :: Integer -> Bool
		  ispal n
		       | n==rev 0 n = True
			   | otherwise = False
			   
		  rev :: Integer -> Integer -> Integer
		  rev a 0 = a
		  rev a m = rev (10*a+mod m 10) (div m 10)
		  
--Problem4
primesIn :: Integer -> Integer -> [Integer]
primesIn l u
         | u<=1 = []
		 | otherwise = pri (seive [2..u]) l
		 
		 
	      where
		  pri :: [Integer] -> Integer -> [Integer]
		  pri (x:xs) l
		     | x<l = pri xs l
			 | otherwise = x:xs 
			 
		  seive :: [Integer] -> [Integer]
		  seive (x:[]) = [x]
		  seive (x:xs) = x : seive ([p|p<-xs, mod p x /=0])
		       
		  	  
		  
--Problem5
isPrime :: Integer -> Bool
isPrime n
       | n<2 = False
	   |otherwise =pchk 2
	   where 
	   --r = div n 2
	   pchk :: Integer -> Bool
	   pchk i
	       |i>=div n 2           = True
	       | mod n i ==0  = False 
		   |otherwise     = pchk (i+1)
		   
--Problem6
decToBin :: Integer -> Integer
decToBin n
        | n == 0   = 0
		|otherwise = 10*decToBin q + r
		where
		q = div n 2
		r= mod n 2
		
--Problem7
binToDec :: Integer -> Integer
binToDec n
        | n==0 =0
		| otherwise = 2*binToDec m + d
		where
		m = div n 10
		d = mod n 10