{- 
 - Examples of recursive code from class on Monday, Jan 13 and
 - Wednesday, Jan. 15.
 - M. Lamb
 -}
module Recursion where



-- returns the factorial of a number
fact :: Int -> Int
fact 1 = 1
fact 0 = 1
fact n
    | n < 0 = error "no negative factorials"
    | otherwise = n * (fact (n-1))


mult a b = a * (b-1)
nums = 1:3:(zipWith (+) nums (tail nums))

check x y = mod (5*x + 2*y) 3 == 0

mystery = map (+3) (5: mystery)


-- determines if its argument is a positive power of 2
isPow2 :: Int -> Bool
isPow2 1 = True
isPow2 0 = False
isPow2 n
    | mod n 2 /= 0 = False
    | otherwise = isPow2 (div n 2)
    
-- sumFunc f low high returns f(low) + f(low+1) + f(low+2) + ... + f(high).
-- If low > high, the result is zero.
sumFunc :: (Int->Int) -> Int -> Int -> Int
sumFunc f low high
    -- This base case was included in class but it's not really needed
    -- | low == high = f low  -- 
    | low <= high = f low + sumFunc f (low+1) high
    | otherwise = 0
    
-- Returns true if its parameter is a prime number
isPrime :: Int -> Bool
isPrime 1 = True
isPrime n | n <= 0 = False
isPrime n = not (hasFactor n 2 (intSqrt n))

-- Helper for isPrime: finds the square root of n, rounded down to an integer
intSqrt :: Int -> Int
intSqrt n
    | n < 0 = error "square root of negative number"
    | otherwise = floor (sqrt (fromIntegral n))

-- A second helper for isPrime.  hasFactor n low high is true if
-- n has a factor in the range [low..high].
hasFactor :: Int -> Int -> Int -> Bool
hasFactor n low high 
    | low > high = False
    | mod n low == 0 = True
    | otherwise = hasFactor n (low+1) high  

-- numPrimes n = number of primes  <= n
numPrimes :: Int -> Int
numPrimes n
    | n < 1 = 0
    | isPrime n = 1 + numPrimes (n-1)
    | otherwise = numPrimes (n-1)
    
-- firstPrimeGreater n = the first prime number greater than  n
firstPrimeGreater :: Int -> Int
firstPrimeGreater n
    | isPrime (n+1) = n+1
    | otherwise = firstPrimeGreater (n+1)

-- smallestFactor n = the smallest prime factor of n (which is also
-- the smallest factor of n -- prime or not -- when n > 1)
smallestFactor :: Int -> Int
smallestFactor n
    | n < 2 = error "n is too small"
    | isPrime n = n
    | otherwise = factorInRange n 2 (intSqrt n)
    
-- helper for smallestFactor: finds the smallest factor of n
-- in the range [low...high]
factorInRange n low high 
    | mod n low == 0 = low
    | otherwise = factorInRange n (low+1) high
   
-- numFactors n finds the number of distinct prime factors of n
-- include duplicates.  So numFactors 36 = 4 because 36 = 2*2*3*3
numFactors :: Int -> Int
numFactors 1 = 0
numFactors n = 1 + numFactors (div n firstFactor)    
    where firstFactor = smallestFactor n

-- distinctFactors n finds the number of distinct prime factors of n
-- include duplicates.  So distinctFactors 36 = 2 because 36 = 2*2*3*3
dictinctFactors :: Int -> Int
dictinctFactors 1 = 0
dictinctFactors n = 1 + dictinctFactors (divideMany n firstFactor)    
    where firstFactor = smallestFactor n
    
-- helper for distinctFactors: divideMany n x is the result of dividing n by
-- x as many times as possible while still getting an integer result
divideMany n x 
    | n == 0 = 0 -- to avoid infinite recursion with zero
    | mod n x == 0 = divideMany (div n x) x
    | otherwise = n

-- hamming n is true if n is a Hamming number: a positive integer that has no other
-- prime factors except 2, 3 and 5.
-- Examples of Hamming numbers: 1, 3, 15, 36, 64
-- Examples of non-Hamming numbers: 7, 35, 55
-- Note: we could have used div instead of divideMany and the results would 
-- be the same.
hamming :: Int -> Bool
hamming 1 = True
hamming n
    | n < 1 = False
    | n == 1 = True
    | mod n 2 == 0 = hamming (divideMany n 2)
    | mod n 3 == 0 = hamming (divideMany n 3)
    | mod n 5 == 0 = hamming (divideMany n 5)
    | otherwise = False
    
-- The in-class exercise from Jan. 15
-- sumDigits n = the sum of the digits in n; for example, sumDigits 123 = 6
-- The problem didn't specify what happens with negative numbers.
sumDigits :: Int -> Int
sumDigits n
    | n < 10 = n
    | otherwise = mod n 10 + sumDigits (div n 10)
    
prefix :: Eq a => [a] -> [a] -> Bool
prefix [] _ = True
prefix _ [] = False
prefix (x:xs) (y:ys)
    | x == y = prefix xs ys
    | otherwise = False

sortedPrefix:: [Int] -> [Int]
sortedPrefix [] = []
sortedPrefix [a] = [a]
sortedPrefix  (x:xs) 
    | x<head(xs) = x : sortedPrefix(xs)
    | otherwise = [x]
    
f (x,y) = (x, ['a'..y])