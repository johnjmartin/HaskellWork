module Friday where
--comment addition
main = putStrLn "Hello World"
isZero :: Int -> Bool
isZero n = (n==0)

absVal :: Int -> Int
absVal n 
	| n < 0 = -n
	| otherwise = n

category n
	| n == 0 = 12
	| n < 0 && mod n 2 == 0 = 2
	| n < 0 && mod n 2 /= 0 = 3
	| n > 0 && mod n 2 == 0 = 4
	| n > 0 && mod n 2 /= 0 = 5  -- or use otherwise

negCat n -- category of n where n <0
	| mod n 2 == 0 = 2
	| otherwise = 3

posCat n -- category
	| mod n 2 == 0 = 4
	| otherwise = 5

newCat 0 = 1
newCat n
	| n < 0 = negCat n
	| otherwise = posCat n

