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