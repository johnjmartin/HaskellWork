{-
  Quiz 2, CISC 260, Winter 2006.
  Code from the quiz plus sample solutions.
-}
module Quiz2 where

-- Question 1A
q1A = map (\x->2*x) (filter (>3) [1,4,5,2,9]) -- value is [8,10,18]

-- Question 1B
f :: Char -> String -> String
f ch str 
    | ('a' <= ch && ch <= 'z') = ch : str
    | otherwise = str
q1B = foldr f "END" "xYz1Ab"

-- Question 1C
q1C = map ((+4).(min 100)) [20,74,101,205] -- value is [24,78,100,100]

-- Question 1D
g x y [] = []
g x y (z:zs) 
  | (x z == y (z+z)) = z:(g x y zs)
  | otherwise = (g x y zs)
-- Try ":type g" in Hugs and you'll get g :: Eq a => (b -> a) -> (b -> a) -> [b] -> [b]  

-- Question 2.  There were many ways to do this.  Most correct answers I saw were variants
-- of one of the following.

-- My initial solution: Uses a special value for EmptyData
data MarksData = EmptyData |
                 BinaryMarksData String Int MarksData MarksData

marksRange :: MarksData -> (Int,Int)
marksRange EmptyData = (100,0)
marksRange (BinaryMarksData _ mark left right) =
  (min mark minChild, max mark maxChild)
  where 
  (leftMin,leftMax) = marksRange left
  (rightMin,rightMax) = marksRange right
  minChild = min leftMin rightMin
  maxChild = max leftMax rightMax
  
-- Solution 2: uses several cases instead of a value for EmptyData
marksRange2 :: MarksData -> (Int,Int)
marksRange2 EmptyData = (0,0) -- could be anything; not used as a base case
marksRange2 (BinaryMarksData _ mark EmptyData EmptyData) = (mark, mark)
marksRange2 (BinaryMarksData _ mark left EmptyData) =
  (min mark leftMin, max mark leftMax)
  where
  (leftMin, leftMax) = marksRange left
marksRange2 (BinaryMarksData _ mark EmptyData right) =
  (min mark rightMin, max mark rightMax)
  where
  (rightMin, rightMax) = marksRange right
marksRange2 (BinaryMarksData _ mark left right) =
  (min mark (min leftMin rightMin), max mark (max leftMax rightMax))
  where
  (leftMin, leftMax) = marksRange left
  (rightMin, rightMax) = marksRange right
  
-- Solution 3: Uses a helper function to compare (min,max) tuples
marksRange3 :: MarksData -> (Int, Int)
marksRange3 EmptyData = (100,0)
marksRange3 (BinaryMarksData _ mark left right) =
  compare (mark,mark) (compare (marksRange3 left) (marksRange3 right))
  where
  compare :: (Int, Int) -> (Int, Int) -> (Int, Int)
  compare (a,b) (c,d)
    | a <= c && b >= d = (a,b)
    | a <= c = (a, d)
    | b >= d = (c,b)
    | otherwise = (c,d)
-- comment: Quite a few students used something equivalent to compare above.  Almost all did it case by
-- the case the way I've got it above.  A simpler version would be:
--   compare (a,b) (c,d) = (min a c, max b d).
-- Only one student did that. 
    
-- Solution 4: collapse the tree into a list of marks & find min and max of the list.
-- A more efficient variant is to create a single function that finds the min & max of a list
-- as a tuple.  A less efficient variant is to sort the list and take the first and last
-- elements.  
marksRange4 :: MarksData -> (Int, Int)
marksRange4 tree = (findMin (collapse tree), findMax (collapse tree))
  where
  collapse :: MarksData -> [Int]
  collapse EmptyData = []
  collapse (BinaryMarksData name mark lc rc) = mark:(collapse lc) ++ (collapse rc)
  findMin :: [Int] -> Int
  findMin [] = 100
  findMin (x:xs) = min x (findMin xs)
  findMax :: [Int] -> Int
  findMax [] = 0
  findMax (x:xs) = max x (findMax xs)
-- comment: findMin is equivalent to foldr min 100, but few people did that.  Similar for findMax  

-- sample tree from the quiz.  marksRange should return (52,95)
testTree1 =
  (BinaryMarksData "Lucy" 95
    (BinaryMarksData "Edmund" 52
      (BinaryMarksData "Beaver" 80 EmptyData EmptyData)
      EmptyData
    )
    (BinaryMarksData "Susan" 73
      (BinaryMarksData "Peter" 85 EmptyData EmptyData)
      (BinaryMarksData "Susan" 90 EmptyData EmptyData)
    )
  )

-- modified sample tree in which the max & min are farther down the tree.
-- marksRange should return (45,100)
testTree2 =
  (BinaryMarksData "Lucy" 95
    (BinaryMarksData "Edmund" 52
      (BinaryMarksData "Beaver" 100 EmptyData EmptyData)
      EmptyData
    )
    (BinaryMarksData "Susan" 73
      (BinaryMarksData "Peter" 45 EmptyData EmptyData)
      (BinaryMarksData "Susan" 90 EmptyData EmptyData)
    )
  )

  
-- Functions for Question 3
evens :: [Int] -> [Int]
evens [] = []
evens (x:xs) 
  | mod x 2 == 0 = x : (evens xs)
  | otherwise = evens xs
  
squares :: [Int] -> [Int]
squares [] = []
squares (x:xs) = (x*x) : (squares xs)
q2 x y z w = w (x y) (z y)

q4 f g x y = ((f.g) x) == y