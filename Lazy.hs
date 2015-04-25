-- Examples used for Lazy Programming topic
-- CISC 260, Winter 2015
-- M. Lamb

module Lazy where

-- Example from slide 3, with end of filter range as a parameter
demo :: Int -> [(Int,Int)]
demo end = zip (map (+5) [1..3]) (filter divBy3 [101..end])   
       where divBy3 x = (mod x 3 == 0)
       
-- Two versions of firstPrime function from slide 4
firstPrimeA :: Int
firstPrimeA = primeStarting 100000
	where
	primeStarting n
	    | isPrime n = n
	    | otherwise = primeStarting (n+1)
        
firstPrimeB :: Int
firstPrimeB = head (filter isPrime [100000..200000])

-- isPrime function for use in other examples (note that lazy evaluation is involved here too!)
isPrime :: Int -> Bool
isPrime n
    | n < 1 = False
    | otherwise = null [factor | factor <- [2..intRoot n],  mod n factor == 0]
    where
    intRoot n = floor (sqrt (fromIntegral n))
    
-- example from slide 5
allTrue :: (a->Bool) -> [a] -> Bool
allTrue p [] = True
allTrue p (x:xs) = (p x) && (all p xs) 
   
-- examples of recursively-defined infinite lists (slide 6)
ones :: [Int]
ones = 1 : ones
infList :: Int -> [Int]
infList n = n : infList (n+7)

-- Slide 7
bigPrime :: Int
bigPrime = 	head [x | x <- [100001..], isPrime x]

firstPrimes :: Int -> [Int]
firstPrimes n = take n (filter isPrime [2..])

-- Slide 8: List of the powers of 3
-- Solution A: correct but inefficient
pow3A :: [Integer]
pow3A = map (3^) [0..]

-- Solution B: more efficient
pow3B :: [Integer]
pow3B = 1 : (map (*3) pow3B)

-- Slide 10: Applying a function to an argument multiple times
series :: (a->a) -> a -> [a]
series f x = x : (map f (series f x))

-- Slide 12: [5,10,20,40,80,160...]
slide12 = 5 : (map (*2) slide12)

-- Slides 14&15: list of factorials
facts1 :: [Integer]
facts1 = map fact [1..]  
  where fact n = product [1..n]
facts2 :: [Integer]
facts2 = 1:(zipWith (*) facts2 [2..])

-- Another way to generate the list of all factorials.
-- Helper function: factStarting n nFact = assume that nFact == n!, returns
--     list of all factorials starting with n!
factStarting :: Integer -> Integer -> [Integer]
factStarting n firstFact = firstFact : (factStarting (n+1) ((n+1) * firstFact))

facts3 = factStarting 1 1

-- Slide 16: approximation of epsilon
eps = 1 + sum (take 10 (map recip facts2)) 
  where   
 	recip :: Integer -> Double  
 	recip n = 1 / (fromIntegral n)
-- more accurate value of epsilon from Haskell: exp 1

-- Slide 17
mystery =  0 : (zipWith (+) mystery [2,4..])

-- Slide 18: Fibonacci numbers
-- 1. VERY inefficient version from slide
fibs1 :: [Integer]
fibs1 = map fib [0..]
    where
    fib :: Int -> Integer
    fib 0 = 0
    fib 1 = 1
    fib n = fib (n-1) + fib (n-2)
    
-- A more efficient version using zipWith
fibs2 :: [Integer]
fibs2 = 0:1:(zipWith (+) fibs2 (tail fibs2))

-- Another efficient version using a helper function 
fibs3 :: [Integer]
fibs3 = 0:1:(fibHelper 0 1)
    where
    -- fibHelper a b assumes a and b are two consecutive Fibonacci numbers (a before b) and returns
    -- the list of all Fibonacci numbers after b
    fibHelper a b = (a+b) : fibHelper b (a+b)
    


    