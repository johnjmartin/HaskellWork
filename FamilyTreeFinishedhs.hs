{-
    A recursive algebraic type for a family tree -- finished version
    CISC 260, winter 2015
    author: M. Lamb
-}
module FamilyTree where
  
data Tree = MakeTree String [Tree]

instance Show Tree where
    show tree = showPerson tree

queen = MakeTree "Elizabeth" [charles, anne, andrew, edward]
charles = MakeTree "Charles" [william,(MakeTree "Harry" [])]
william = MakeTree "William" [MakeTree "George" []]
anne = MakeTree "Anne" [peter,zara]
peter = MakeTree "Peter" [MakeTree "Savannah" [], MakeTree "Isla" []]
zara = (MakeTree "Zara" [MakeTree "Mia" []])
andrew = MakeTree "Andrew" [(MakeTree "Beatrice" []),(MakeTree "Eugenia" [])]
edward = MakeTree "Edward" [(MakeTree "Louise" []),(MakeTree "James" [])]

-- Another family tree as one big expression (George was my grandfather)
george = MakeTree "George" [
    MakeTree "Helen" [
      MakeTree "Margaret" [
        MakeTree "Carolyn" [],
        MakeTree "Valerie" [],
        MakeTree "Ian" []
      ],
      MakeTree "Barbara" []
    ],
    MakeTree "Dorothy" [
      MakeTree "Janet" [],
      MakeTree "Danny" [
        MakeTree "Tiffani" [],
        MakeTree "Dana" []
      ],
      MakeTree "Jimmy" [MakeTree "James" []]
    ]
  ]

getName :: Tree -> String
getName (MakeTree name _) = name

showPerson :: Tree -> String
showPerson person = showPersonIndented 0 person

indent :: Int -> String
indent 0 = ""
indent n = "    " ++ (indent (n-1))

showPersonIndented :: Int -> Tree -> String
showPersonIndented level (MakeTree name []) =
    (indent level) ++ name
showPersonIndented level (MakeTree name kids) =
    (indent level) ++ name ++ "\n" ++ (showPeopleList (level+1) kids)
    
showPeopleList :: Int -> [Tree] -> String
showPeopleList _ [] = ""
showPeopleList level [onePerson] = showPersonIndented level onePerson
showPeopleList level (person:morePeople) = 
    (showPersonIndented level person) ++ "\n" ++
    (showPeopleList level morePeople)
    
childNames :: Tree -> [String]
childNames (MakeTree _ kids) = map getName kids
    where
    getName (MakeTree name _) = name
    
allNames :: Tree -> [String]
{-
allNames (MakeTree name kids) = name : (namesFromList kids)
namesFromList :: [Tree] -> [String]
namesFromList [] = []
namesFromList (tree:trees) = (allNames tree) ++ (namesFromList trees)
-}
allNames (MakeTree name kids) = name : (concat (map allNames kids))

search :: Tree -> String -> Bool
search (MakeTree name kids) target
    | name == target = True
    | otherwise = or (map ((flip search) target) kids)
    
-- Adds a new person to a family tree, producing a new family tree
-- Parameters: family tree, name of parent, name of child
-- Assumption: no duplicate names allowed
-- If the parent is not in the family tree, the tree is unchanged.
addDescendent :: Tree -> String -> String -> Tree
addDescendent (MakeTree name kids) parentName childName
    | name == parentName = (MakeTree name (kids ++ [newKid]))
    | otherwise = MakeTree name
        -- If the parent is in the family tree, this map will change only
        -- that parent's part of the tree.  Since there are no duplicate names,
        -- the new child will only be added once.
        (map (\p->addDescendent p parentName childName) kids)
    where
    newKid = (MakeTree childName [])



