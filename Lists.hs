-- Examples used with Lists topic
-- CISC 260, Winter 2015

module Lists where
import Data.Char -- for toUpper and isAlpha functions

-- Returns the second element in a list.  Error if
-- the list doesn't contain at least two elements.
second :: [a] -> a
second lis = head (tail lis)

-- Version 2, using a cons pattern instead of head & tail
second2 :: [a] -> a
second2 (x:xs) = head xs

-- Creates a copy of a string with all lower-case letters
-- changed to upper case.

-- Version A, using head & tail
capitalizeA :: String -> String
capitalizeA "" = ""
capitalizeA str = (toUpper (head str)) : (capitalizeA (tail str))

-- Version B, using a pattern
capitalizeB :: String -> String
capitalizeB "" = ""
capitalizeB (firstChar:theRest) = (toUpper firstChar):(capitalizeB theRest)

-- Version C, using a list comprehension
capitalizeC :: String -> String
capitalizeC str = [toUpper ch| ch <- str]

-- Same as capitalize functions but also leave any non-letter characters out
-- of the result

-- Version A, using recursion
caps2A :: String -> String
caps2A "" = ""
caps2A (c:cs)
    | isAlpha c = (toUpper c):(caps2A cs)
    | otherwise = (caps2A cs)
    
-- Version B, using a pattern
caps2B :: String -> String
caps2B "" = ""
caps2B (firstChar:theRest)
    | isAlpha firstChar = (toUpper firstChar):(caps2B theRest) 
    | otherwise = caps2B theRest
 
-- Version C, using a list comprehension
caps2C :: String -> String
caps2C str = [toUpper ch| ch <- str, isAlpha ch]

 
-- Returns the first odd number from a list of numbers.
-- Error if list contains no odd numbers

-- Version A, using recursion
firstOddA :: [Int] -> Int
firstOddA (x:xs)
    | mod x 2 /= 0 = x
    | otherwise = firstOddA xs
    
-- Version, using a list comprehension
firstOddB :: [Int] -> Int
firstOddB nums = head [n | n<-nums, mod n 2 /= 0]

-- re-write head & tail functions
newHead :: [a] -> a
newHead (x:xs) = x
newHead _ = error "head of empty list"
newTail :: [a] -> [a]
newTail [] = error "tail of empty list"
newTail (x:xs) = xs

-- detect singleton list
isSingle :: [a] -> Bool
isSingle [_] = True
isSingle _ = False

-- append list1 list2 should be the same as list1 ++ list2
-- A good exercise and a reminder of the differences between
-- ":" and "++"
append [] list = list
append (x:xs) list = x : (append xs list)