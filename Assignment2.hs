--By Hannah Wilkinson (10026110) and John Martin (10085202)

module Assignment2 where

--hopscoth calls the function paths where the output of paths then is received by bestPath
--which returns the result to hopscotch as a tuple with the best path, as well as the sum of this list
hopscotch:: [Integer] -> ([Integer], Integer)
hopscotch result = bestPath( paths result)

-- paths takes a hopscotch course (a list of squares described by number of points) as a parameter and returns
-- a list of all the sequences of squares which are legal paths through the course.
-- Example: paths [1,2,3,4,5,6] should be [[1,3,5],[1,3,6],[1,4,6],[1,4]] (by Professor Lamb)
paths :: [Integer] -> [[Integer]]
paths(x:y:z:zs) = map (x:)(paths(z:zs)) ++ map (x:) (paths(zs))
paths(x:y) = [[x]] 
paths [] = [[]]

--bestPath takes a list of all the lists created by the paths function and returns the list with the 
--largest sum and its corresponding sum as a tuple
bestPath :: [[Integer]]-> ([Integer], Integer)
bestPath[x] = (x, sum(x))
bestPath [x,y]
	| sum(x) >= sum(y) = (x, sum(x))
	| sum(y) > sum(x) = (y, sum(y))
bestPath (x:y:ys)
	| sum(x) <= sum(y) = bestPath(y:ys)
	| otherwise = bestPath(x:ys) 
  