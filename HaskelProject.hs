data Item = I String deriving (Show,Eq)
data User = U String deriving (Show,Eq)
data Rating a =  NoRating | R a deriving(Show,Eq,Ord)

ratingnum NoRating =0.0
ratingnum (R a) =a



dis :: Eq a => [a] -> [a] 
dis []=[]
dis (x:xs) | (elem x xs) = dis xs
			| otherwise =x:(dis xs)
			

fromRatingsToItems :: Eq a => [(b,a,c)] -> [a] 
fromRatingsToItems []=[]
fromRatingsToItems ((a,b,c):xs)= dis(k) where k=  b:fromRatingsToItems(xs)
			
fromRatingsToUsers :: Eq a => [(a,b,c)] -> [a]
fromRatingsToUsers []=[]
fromRatingsToUsers ((a,b,c):xs)= dis(k) where k=  a:fromRatingsToUsers(xs)		

hasRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Bool
hasRating _ _ []=False
hasRating a b ((c,d,e):xs) | (a==c && b==d) = True
							| otherwise = (hasRating a b xs)
							
getRating :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> c							
getRating _ _ []=error "No given rating"
getRating a b ((c,d,e):xs) | (a==c && b==d) = e
							| otherwise = (getRating a b xs)

formMatrixUser :: (Eq a, Eq b, Fractional c) => b -> [a] -> [(b,a,c)] -> [Rating c]							
formMatrixUser _ [] _= []
formMatrixUser a (x:xs) z = if (hasRating a x z)==True  then (((R(getRating a x z))):(formMatrixUser a xs z)) 
							else  NoRating:(formMatrixUser a xs z)
							
							
formMatrix :: (Eq a, Eq b, Fractional c) => [b] -> [a] -> [(b,a,c)] -> [[Rating c]]
formMatrix [] _ _=[]
formMatrix (a:as) x z =  (formMatrixUser a x z ):(formMatrix as x z)

numberRatingsGivenItem :: (Fractional a, Num b) => Int -> [[Rating a]] -> b 
numberRatingsGivenItem _ []=0
numberRatingsGivenItem n (x:xs) | (x!!n) == NoRating =  numberRatingsGivenItem n xs
								| otherwise = 1 + numberRatingsGivenItem n xs
 

differeneRatings :: Fractional a => Rating a -> Rating a -> a
differeneRatings NoRating _=0.0
differeneRatings _ NoRating =0.0
differeneRatings (R x)(R y)=x-y

matrixPairs :: Num a => a -> [(a,a)] 
matrixPairs n= helpera 0 0 (n-1) 
helpera n m x | (n==x && m==x) = [(n,m)]
					| (n==x && m/=x)= ([(n,m)]++(helpera n (m+1) x))
						| (m==x && n/=x)=([(n,m)]++(helpera(n+1) 0 x))
							| otherwise = ([(n,m)]++(helpera n (m+1) x))
							
dMatrix :: Fractional a => [[Rating a]] -> [a] 
dMatrix (x:xs)= (dMatrixH1  z (x:xs) )              where z=matrixPairs (length x)

dMatrixH1 [] _=[]
dMatrixH1 ((a,b):xs) z=(dMatrixH (a,b) z)++(dMatrixH1 xs z)

dMatrixH  (a,b) []  = []
dMatrixH (a,b) (x:xs) =[sum m] where m=([(differeneRatings (x!!a) (x!!b))]++(dMatrixH(a,b) xs ))

--freqMatrix :: (Num a, Fractional b) => [[Rating b]] -> [a]
freqMatrix (x:xs) =  (freqMatrixH1 z (x:xs))           where z=matrixPairs (length x)
freqMatrixH1 [] _=[]
freqMatrixH1 ((a,b):xs) z= [(freqMatrixH (a,b) z)]++(freqMatrixH1 xs z)

freqMatrixH (a,b) z=(min (numberRatingsGivenItem a z) (numberRatingsGivenItem b z))

--diffFreqMatrix :: Fractional a => [[Rating a]] -> [a] 
diffFreqMatrix x=  (div2 (dMatrix x) (freqMatrix x)) 
div2 [] [] =[]
div2 (x:xs)(y:ys)=(x/y):div2 xs ys


predict l a b   |ratingnum((k!!a)!!b)==0.0 = final
				| otherwise = ratingnum ((k!!a)!!b)
			where 
				z = length( fromRatingsToUsers l) * (b+1)
				k = formMatrix ( fromRatingsToUsers l)(fromRatingsToItems l) l
				nI= length (fromRatingsToItems l)
				f=predictH 0 (b*nI) nI (diffFreqMatrix k)
				p=predictH2 (k!!a) f
				ps= sum p
				d= numofitemsrated (k!!a)
				final= noneed ps d

predictH a b c (x:xs) | a==b = predictH1 (x:xs) c
					  | otherwise =(predictH (a+1) b c xs)

predictH1 l 0  = []
predictH1 (x:xs) n = x:predictH1 xs (n-1)
					   

predictH2 [] [] =[]			
predictH2 (x:xs) (a:as) = (z+a):(predictH2 xs as)
						where z= ratingnum x
noneed x y =x/ (fromIntegral y)

numofitemsrated []=0
numofitemsrated (x:xs)= if x/= NoRating then 1+numofitemsrated xs
						else numofitemsrated xs
						
				




