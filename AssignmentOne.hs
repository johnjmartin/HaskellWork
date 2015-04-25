module AssignmentOne where
-- Function 1
-- searching for a positive integer n for which the sum of all of the factors of n is equal to 2n
isPerfect :: Int -> Bool
isPerfect n
        | n <= 0 = False
        | sumOfFactors n n == 2*n = True
        | otherwise = False

sumOfFactors :: Int -> Int -> Int
sumOfFactors n divisor
        | divisor == 1 = 1
        | mod n (divisor) == 0 = sumOfFactors n (divisor-1) + divisor
        | otherwise = sumOfFactors n (divisor-1)

-- return the nth positive integer for which the predicate is true.
nthTrue :: (Int -> Bool) -> Int -> Int
nthTrue func n
	|n < 1 = error "Invalid integer"
	|funcTest func 0 n 0

funcTest :: (Int -> Bool) -> Int -> Int -> Int -> Int
funcTest func Ccount Dcount num
	| Ccount == Dcount = num
	| func num == True = funcTest (func) (Ccount + 1) (Dcount) (num + 1 ) 
	| func num == False = funcTest (func) (Ccount) (Dcount) (num + 1 )


