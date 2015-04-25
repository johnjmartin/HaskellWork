--Class Notes
-- W1L3
module Friday where

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

--Lec 2 Week 2
--recursion
module Recursion where
	
	factorial 0 = 1
	factorial n | n < 0 = error "neg not defined"
	factorial n = n * factorial n-1

	pow2 :: Int -> Bool
	pow2 1 = True
	pow2 n 
		| mod n 2 == 0 = pow2 (div n 2)
		| otherwise = False

	sumfunc :: (Int -> Int) -> Int -> Int ->Int
	sumfunc f low high
		| low > high = 0
		| otherwise = (f low) + (sumfunc f (low +1) high)

	hasFactor n low high
		| low > high =False
		| mod n low == 0 = True
		| otherwise = hasFactor n (low+1) high

	isPrime n 
		| n < 1 = False
	isPrime n = not (hasFactor n 2 (div n 2 ))

	primesLeq n
		| n < 2 = 0
		| isPrime n = 1 + primesLeq (n-1)
		| otherwise = primesLeq (n-1)

	firstPrime n
		| isPrime n = n
		| otherwise = firstPrime (n+1)

	--smallest prime factor finder
	lowPrimeInRange low high 
		| low > high = error "no primes in range"
		| isPrime low = low
		| otherwise = lowPrimeInRange (low+1) high
		
	isHamming

--W3L2 missed
-- W3L3
 append :: [a] -> [a] -> [a]
 append [] list = list
 append list [] = list
 append (x:xs) list = x:(append xs list)

--W4L1 tuples
module WeekfourLectureone where
	-- lists and tuples
	-- code should be on video
	
