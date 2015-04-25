{-
 - Starting code for Assignment 3
 - CISC 260, Winter 2015
 - M. Lamb
 -
 - This file contains a few things to get you started on Assignment 3.
 - The main things are a type definition and a show function for a binary 
 - search tree of Ints.  You **must** leave these two items in this file
 - without changes; for consistent testing it's important that the type
 - definition and show functions are not changed.
 -
 - I have also included a sample tree, built "from scratch" without the
 - add function that you will be implementing.  It may be useful for
 - implementing and testing some simpler functions before you've got 
 - your add function working.  You may leave the sample tree in your
 - file or not; up to you.
 - 
 - Re-write this comment to reflect the fact that what you
 - are handing in is a finished solution and not starting code.  
 - Remove the instructions and add your name(s) to mine as additional author(s).  
 -}

module Assignment3 where

-- Algebraic type for a binary search tree of integers.  A tree may be empty, or it 
-- may have a root, a left subtree and a right subtree.  (Note that one or both of the
-- subtrees might be empty.)  

data Tree = Empty | MakeTree Tree Int Tree

-- Creates a multi-line sideways representation of a tree.  The root will be on the left,
-- with its left child below it and its right child above it.  If you tilt your head to the
-- left you'll see the structure of the tree.
-- The second parameter is the indentation level.
treeToString :: Tree -> Int -> String
treeToString Empty _ = ""
treeToString (MakeTree left root right) indent =
    rightString ++ 
    (spaceString indent) ++ (show root) ++ "\n" ++ 
    leftString
    where
    leftString = treeToString left (indent+4)
    rightString = treeToString right (indent+4)
    
-- Creates a string consisting of n spaces (assuming n >= 0)
spaceString :: Int -> String
spaceString 0 = ""
spaceString n = ' ':(spaceString (n-1))

-- treeToString will be used to display trees
instance Show Tree where show tree = treeToString tree 0

-- A legal binary search tree created by hand for your use.  It's
-- a pain to create a tree this way and normally you could create a
-- tree using the add function.  But I'm providing this tree so that
-- you have a tree to practice with even before your add function 
-- is working.
sampleTree = 
    (MakeTree -- tree with root 45
        (MakeTree -- tree with root 15
            (MakeTree -- tree with root 4
                (MakeTree Empty (-1) Empty)
                4
                (MakeTree Empty 7 Empty)
            )
            15
            (MakeTree Empty 23 Empty)
        )
        45
        (MakeTree -- tree with root 72
            Empty
            72
            (MakeTree -- tree with root 103
                (MakeTree Empty 99 Empty)
                103
                Empty
            )
        )
    )
            


