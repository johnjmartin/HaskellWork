module Algebraictypes where
--a student has a name, an ID number, and a list of marks
data student = CreateStudent String Int [Float]
	deriving (Show, Eq)
instance Eq student
	where
	(CreateStudent name1 _ _) == (CreateStudent name2 _ _)
		|name1 == name2 = True
		|otherwise = False

data Person = CreateStudent String Int [Float] | CreateProf String Bool
	deriving (Show)

instance Eq Person
	where
	(CreateStudent name1 _ _) == (CreateStudent name2 _ _)
		|name1  == name2 =True
		|otherwise=False	
	(CreateProf name1 _) == (CreateProf name2 _)
		| name1 == name2 = True
		|otherwise = False
	x == y = False


data Prof = CreateProf String Bool
snape = CreateProf "Snape" False
harry = CreateStudent "harry" 123 [84.5]


harry = CreateStudent "Harry" 123 [82.5, 55.2, 72.3]
harry2 = CreateStudent "Harry" 123 [82.5, 55.2, 72.3]
ron = CreateStudent "Ron" 123 [82.5, 78.4]

getName :: student ->String
getName (CreateStudent name _ _) = name
addMark :: student ->Float->student
addMark (CreateStudent name id markList) newMark = (CreateStudent name id (markList ++ [newMark])

idMatch :: student -> student -> Bool
idMatch (CreateStudent _ id1 _ ) (CreateStudent _ id2 _) = id1 == id2

--class 02
-- work with trees













