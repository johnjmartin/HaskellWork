module Quiz1 where

-- QUESTION 1: Three different solutions, all correct.  Most correct
-- student answers looked a lot like one of these.
isGreen1 :: Int -> Bool
isGreen1 x 
  | x == 0 = error "called isGreen1 with zero"
  -- | x < 0 = isGreen1 (0-x) -- this case isn't necessary, but no harm
  | mod x 2 /= 0 = True -- divisible by 2 zero times
  | otherwise = not (isGreen1 (div x 2)) -- x/2 should be divisible by 2 an odd number of times
  
isGreen2 :: Int -> Bool
isGreen2 x 
  | x == 0 = error "called isGreen2 with zero"
  -- | x < 0 = isGreen2 (0-x) -- this case isn't necessary, but no harm
  | otherwise = (mod (twoCount x) 2) == 0
twoCount:: Int-> Int
twoCount n -- number of times you can divide n by 2 evenly (even for "green" numbers only)
	| mod n 2 /= 0 = 0
    | otherwise = 1 + twoCount (div n 2)
    
isGreen3 :: Int -> Bool
isGreen3 x 
  | x == 0 = error "called isGreen3 with zero"
  -- | x < 0 = isGreen3 (0-x) -- test case isn't necessary, but no harm
  | mod x 2 /= 0 = True -- divisible by 2 zero times
  | mod x 4 /=0 = False -- divisible by 2 exactly once
  | otherwise = isGreen3 (div x 4)  -- divide by 2 twice and try again

-- Two testing functions for the isGreen versions: not required, but they were
-- useful in debugging and I've left them in.

-- tests an isGreen function with a list of green numbers
testGreen :: (Int->Bool) -> Bool
testGreen f = and (map f [1,-1,4,-4,5,-5,12,-12,64,-64,80,-80,320,-320]) 
-- tests an isGreen function with a list of non-green numbers
testNotGreen :: (Int->Bool) -> Bool
testNotGreen f = not (or (map f [2,-2,6,-6,8,-8,24,-24,150,-150]))

-- QUESTION 2.  Defining and using the Pairs type was optional.
type Pairs = [(Float, Float)]

-- recursive solution
squareCount1 :: Pairs -> Int
squareCount1 [] = 0
squareCount1 ((a,b):morePairs)
  | b == (a*a) = 1 + squareCount1 morePairs
  | otherwise = squareCount1 morePairs
  
-- solution using a list comprehension
squareCount2 :: Pairs -> Int
squareCount2 pairs = length [(a,b) | (a,b)<-pairs, b == (a*a)]
-- Note about line above: (a,b) before line could be replaced by a, or 1,
-- or just about anything, since all we need is the length.

-- another list comprehension solution
squareCount3 :: Pairs -> Int
squareCount3 pairs = sum[1 | (a,b)<-pairs, b == (a*a)]

-- tests a squareCount function with several lists
testSC :: (Pairs->Int) -> Bool
testSC f = 
  (map f [ [(1,2),(2,4),(4,7),(-5,25)], [(9,3)], [(4,16),(5,2)] ]) == [2,0,1]
	
-- QUESTION 3

sortedPrefix :: [Int] -> [Int]
sortedPrefix [] = []
sortedPrefix [x] = [x]
sortedPrefix (a:b:rest)
  | a <= b = a : sortedPrefix (b:rest)
  | otherwise = [a]
	
-- Or, if the pattern (a:b:rest) bothers you:
sortedPrefix2 :: [Int] -> [Int]
sortedPrefix2 [] = []
sortedPrefix2 [x] = [x]
sortedPrefix2(x:xs)
  | x <= (head xs) = x : sortedPrefix2 xs
  | otherwise = [x]
	
-- Quite a few solved this question using a helper function:
sortedPrefix3 :: [Int] -> [Int]
sortedPrefix3 [] = []
sortedPrefix3 (x:xs) = helper x xs
  where
	helper x [] = [x]
	helper x (y:ys) 
	  | x > y = [x]
		| otherwise = x : helper y ys
	
-- tests a sortedPrefix function with several lists
testSP :: ([Int]->[Int]) -> Bool
testSP f = (map f [[1,5,14,13,15,19],[1,1,2,2,3,3,2],[3,2,1],[42],[]]) == [[1,5,14],[1,1,2,2,3,3],[3],[42],[] ]

-- QUESTION 4
sortedSeq :: [Int] -> [Int]
sortedSeq [] = []
-- Longest sortest sublist is either a prefix, or else
-- a sublist of the tail of the list
sortedSeq (a:rest) =
  longestString (sortedPrefix (a:rest)) (sortedSeq rest)
  where 
  longestString s1 s2 
    | length s1 > length s2 = s1
    | otherwise = s2
		
-- A messier but more efficient solution.  Instead of trying
-- every tail of the list, drop the entire sorted prefix and
-- continue with what's left.
sortedSeq2 :: [Int] -> [Int]
sortedSeq2 [] = []
sortedSeq2 lis 
	| length1 > length2 = sublist1
	| otherwise = sublist2
	where
	sublist1 = sortedPrefix lis -- longest prefix of list
	length1 = length sublist1 
	remainder = drop length1 lis -- list with that prefix chopped off the front
	sublist2 = sortedSeq2 remainder
	length2 = length sublist2 -- longest sublist of the remaining list
	
-- I didn't think of this one, but several students did, although I think nobody
-- got it entirely right.  The idea is to create a list of *all* the sorted
-- sublists, and then find the longest of those.
sortedSeq3 lis = longest (allSeqs lis)
  where
	allSeqs :: [Int] -> [[Int]] -- from list to list of sorted sublists
	allSeqs [] = []
	allSeqs [x] = [[x]]
	allSeqs (x:xs) = (sortedPrefix (x:xs)) : (allSeqs xs)
	longest :: [[Int]] -> [Int] -- chooses longest list from list of lists
	longest [] = []
	longest [lis] = lis
	longest (lis:remainder)
		| (length lis) > (length (longest remainder)) = lis
		| otherwise = longest remainder
	

		
		
  

