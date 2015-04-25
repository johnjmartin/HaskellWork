-- Examples of basic algebraic types
-- Discussed in class Feb. 24.  This version is
-- cleaned up and includes a few extra functions
-- for illustration.

-- CISC 260, Winter 2015
-- M. Lamb

module AlgTypesExamples where

-- A person can be either a student or a prof.
-- A student consists of a name, an id number, and a list of marks.
-- A prof consists of a name and a boolean telling us whether they have tenure.
data Person = CreateStudent String Int [Float] | CreateProf String Bool
    deriving Show
	
-- The "deriving Show" above means that the Person type is part of the 
-- Show class and that Haskell should create a simple show function for 
-- the type.  If we want to specify how Persons are displayed we should
-- use "instance Show Person" and define our own show function -- the way
-- we defined the meaning of the "==" operator below.
    
-- Two people are considered equal if they are the same type of
-- person (two students or two profs) and have the same name.

-- Using "instance" like this means the Person type is a member
-- of the Eq class and also tells Haskell what the meaning of "=="
-- is for Persons.  The Eq class also contains a "\=" member,
-- but Haskell is smart enough to know that "\=" is the opposite
-- of "==", so we don't have to define "\=" as well.
instance Eq Person
    where
    (CreateStudent name1 _ _) == (CreateStudent name2 _ _) 
        | name1 == name2 = True
        | otherwise = False
    (CreateProf name1 _) == (CreateProf name2 _) 
        | name1 == name2 = True
        | otherwise = False
    x == y = False
	
-- Some sample people to experiment with
harry = CreateStudent "Harry" 123 [82.5,78.4]
harry2 = CreateStudent "Harry" 123 [82.5,78.5]
ron = CreateStudent "Ron" 123 [82.5,78.4]
hermoine = CreateStudent "Hermoine" 456 [92.5, 96.8, 99.9]
snape = CreateProf "Snape" False

-- Returns a person's name.  Works for both students and profs.
getName :: Person -> String
getName (CreateStudent name _ _) = name
getName (CreateProf name _) = name

-- Returns a student's average.  Writes an error message if the
-- parameter is a prof, or a student with no marks.
average :: Person -> Float
average (CreateStudent _ _ []) = error "student with no marks has no average"
average (CreateStudent _ _ marks) = (sum marks) / (fromIntegral (length marks))
average _ = error "profs have no marks" 





