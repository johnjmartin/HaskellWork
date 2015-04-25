module Practice where
	--List practice:
	maxOccurs::Integer -> Integer -> (Integer, Integer)
	maxOccurs x y
		| x == y = (y, 2)
		| x > y = (x, 1)
		| otherwise = (y,1)

	orderTriple :: (Integer,Integer,Integer) -> (Integer,Integer,Integer)
	orderTriple (x,y,p) = (x,y,p)

	newLength :: [Integer] -> [Integer]
	newLength ns =  map (square) ns
		where
			square x = x * x

	sumSquares :: [Integer] -> Integer
	sumSquares ns =  sum (newLength ns)

	zeroCheck :: [Integer] -> Bool
	zeroCheck ns
		|(filter (square) ns) == [] = False
		|otherwise = True
		where
			square x = x > 0

	tenThirteen :: [Integer] -> Integer
	tenThirteen x = foldr1 (+) (map (square) x) 
		where
			square x = x * x

	addnum :: Integer -> (Integer->Integer)
	addnum n = (\m -> n+m)

	maxFunc f1 f2 x
		| f1 x > f2 x = f1 x
		| otherwise = f2 x
	---practice problem A
	---foldpairs :: (b->b->b) -> [b] ->[b]
	foldpairs fun [] = []
	---foldpairs fun lis = (length(lis) % 2) == 1 = 0
	foldpairs fun (x:y:ys)  = (fun x y) : foldpairs fun ys


	---practice problem D

	f1 x = x + 6
	f2 x = x*x - 1
	f3 = maxFunc f1 f2
	
	q2 x y z w = w (x y) (z y)

	monday :: Int -> Int -> Int
	monday a b = 2*b - a

	f1a x = x*x
	f2b = f1a . (10-)

	mix :: (Int,Char) -> String -> String
	mix (0,c) str = str
	mix (n,c) str = mix (n-1,c) (c:str)
	answerD = foldr mix "ABC" [(1,'X'),(3,'T'),(2,'S')]
	q3E x y z = (x+z):y
	q3F x y z w = y (z w x) 

	fasb x = x^2-1
	gab = fasb.(+3)

	h = (7-)
	q1 [] x = x
	q1 (x:xs) y = q1 xs (x-y)
	k :: Int -> Float -> Float
	k x y = (fromIntegral x) + y

	g x y [] = []
	g x y (z:zs)
		| (x z == y z) = z:(g x y zs)
		| otherwise = (g x y zs) 

	g1 x y z = (x+y, z)

	g2 a b c
		| a > head b = a : (head b) : (head c)
		| otherwise = (head b) : (head c)

	g3 a b c d = (b d):(map a c)

	fq1d :: Char -> String -> String 
	fq1d ch str
		| ('a' <= ch && ch <= 'z') = ch : str
		| otherwise = str

	first (x,_) = x
	newfunc = curry first
	solve w x y z
		| w y < w z = x (x y z) z
		| otherwise = x y (x y z) 

	