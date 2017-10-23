{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude #-} 

module Main where
import Prelude hiding (takeWhile,all)
import Test.HUnit      -- unit test support

import XMLTypes        -- support file for XML problem (provided)
import Play            -- support file for XML problem (provided)

doTests :: IO ()
doTests = do 
  _ <- runTestTT $ TestList [ testFoldr, testTree, testXML ]
  return ()

main :: IO ()
main = do 
       doTests
       return ()

----------------------------------------------------------------------

testFoldr :: Test
testFoldr = TestList [tintersperse, tinvert, ttakeWhile, tfind, tall]

-- The intersperse function takes an element and a list 
-- and `intersperses' that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

intersperse ::  a -> [a] -> [a]
intersperse sep = foldr (\x l -> if null l then [x] else x:sep:l) []
tintersperse :: Test
tintersperse = "intersperse" ~: TestList [intersperse ',' "abcde" ~?= "a,b,c,d,e",
                                          intersperse 2 [1, 3]        ~?= [1, 2, 3],
                                          intersperse 3 [4]           ~?= [4]]


-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)] returns [(1,"a"),(2,"a")] 

invert :: [(a,b)] -> [(b,a)]
invert = map (\(x, y) -> (y, x))
tinvert :: Test
tinvert = "invert" ~: TestList [invert [("a", 1), ("a", 2)]  ~?= [(1, "a"), (2, "a")],
                                invert ([] :: [(Int, Char)]) ~?= []]
 

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile p list = reverse $ fst $ foldl take' ([], True) list
  where
    take' (l, False) _ = (l, False)
    take' (l, True) x = if p x then (x:l, True) else (l, False)
ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: TestList [takeWhile (< 3) [1, 2, 3, 4, 1, 2, 3, 4] ~?= [1, 2],
                                      takeWhile (< 9) [1, 2, 3] ~?= [1, 2, 3],
                                      takeWhile (< 0) [1, 2, 3] ~?= []]
 

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

find :: (a -> Bool) -> [a] -> Maybe a
find p = foldl f Nothing
  where
    f (Just x) _ = Just x
    f Nothing x = if p x then Just x else Nothing

tfind :: Test
tfind = "find" ~: TestList [find odd [0, 2, 3, 4]      ~?= Just 3,
                            find odd [0, 2, 4, 20, -2] ~?= Nothing,
                            find odd []                ~?= Nothing]
 

-- all pred lst returns False if any element of lst 
-- fails to satisfy pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

all  :: (a -> Bool) -> [a] -> Bool
all p = foldl (\acc x -> acc && (p x)) True

tall :: Test
tall = "all" ~: TestList [all odd [1, 2, 3]  ~?= False,
                          all odd [1, 3, -5] ~?= True,
                          all odd []         ~?= True]
 

----------------------------------------------------------------------

testTree :: Test
testTree = TestList [ tinvertTree, ttakeWhileTree, tallTree, tmap2Tree, tzipTree ]

-- | a basic tree data structure
data Tree a = Leaf | Branch a (Tree a) (Tree a) deriving (Show, Eq)

foldTree :: b -> (a -> b -> b -> b) -> Tree a -> b
foldTree e _ Leaf     = e
foldTree e n (Branch a n1 n2) = n a (foldTree e n n1) (foldTree e n n2)

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f = foldTree Leaf (\x t1 t2 -> Branch (f x) t1 t2) 

-- The invertTree function takes a tree of pairs and returns a new tree 
-- with each pair reversed.  For example:
--     invertTree (Branch ("a",1) Leaf Leaf) returns Branch (1,"a") Leaf Leaf

treeLift :: a -> Tree a
treeLift x = Branch x Leaf Leaf

rootTree :: Tree Int
rootTree = treeLift 99

smallTree :: Tree Int
smallTree = Branch 3 Leaf (treeLift 1)

llTree :: Tree Int
llTree = Branch 1 Leaf (Branch 2 Leaf (Branch (-3) Leaf (treeLift 7)))

miscTree :: Tree Int
miscTree = Branch 5 (Branch 3 (treeLift 6) Leaf) (Branch (-1) (treeLift 3) (treeLift 20))

invertTree :: Tree (a,b) -> Tree (b,a)
invertTree = mapTree (\(x, y) -> (y, x))
tinvertTree :: Test
tinvertTree = "invertTree" ~: TestList [invertTree (treeLift (3, 4)) ~?= treeLift (4, 3),
                                        invertTree (Branch (1, 2) Leaf (treeLift (6, 4))) ~?= Branch (2, 1) Leaf (treeLift (4, 6))]

 

-- takeWhileTree, applied to a predicate p and a tree t, 
-- returns the largest prefix tree of t  (possibly empty) 
-- where all elements satisfy p. 
-- For example, given the following tree

tree1 :: Tree Int
tree1 = Branch 1 (Branch 2 Leaf Leaf) (Branch 3 Leaf Leaf)

--     takeWhileTree (< 3) tree1  returns Branch 1 (Branch 2 Leaf Leaf) Leaf
--     takeWhileTree (< 9) tree1  returns tree1
--     takeWhileTree (< 0) tree1  returns Leaf

takeWhileTree :: (a -> Bool) -> Tree a -> Tree a
takeWhileTree f t = foldTree Leaf take' t
  where
    -- take' :: (a -> Tree a -> Tree a -> Tree a) 
    take' x left right = if f x then Branch x left right else Leaf

ttakeWhileTree :: Test
ttakeWhileTree = "takeWhileTree" ~: TestList [takeWhileTree (< 3) tree1 ~?= Branch 1 (treeLift 2) Leaf,
                                              takeWhileTree (< 9) tree1 ~?= tree1,
                                              takeWhileTree (< 0) tree1 ~?= Leaf]
 

-- allTree pred tree returns False if any element of tree 
-- fails to satisfy pred and True otherwise.
-- for example:
--    allTree odd tree1 returns False

allTree :: (a -> Bool) -> Tree a -> Bool
allTree f t = foldTree True (\x left right -> (f x) && left && right) t
tallTree :: Test
tallTree = "allTree" ~: TestList [allTree (< 3) tree1 ~?= False,
                                  allTree (< 9) tree1 ~?= True,
                                  allTree (< 0) tree1 ~?= False]
 

-- WARNING: This one is a bit tricky!  (Hint: the value
-- *returned* by foldTree can itself be a function.)

-- map2Tree f xs ys returns the tree obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one branch is longer than the other, then the extra elements 
-- are ignored.
-- for example:
--    map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) (Branch 3 Leaf Leaf)
--        should return (Branch 4 Leaf Leaf)

-- The idea is to fold over the first tree, creating a function which takes in
-- the second tree and returns the result. These functions are defined recursively
-- at the node level - the base case for the first tree is to return const Leaf,
-- and these node level functions call eachother recurisvely. If any of these node
-- functions see a Leaf as an input, they return instead of calling recursively
map2Tree :: (a -> b -> c) -> Tree a -> Tree b -> Tree c
map2Tree f = foldTree (const Leaf) f'
  where
    -- f' :: a -> (Tree b -> Tree c) -> (Tree b -> Tree c) -> (Tree b -> Tree c)
    f' _ _ _ Leaf = Leaf
    f' x fLeft fRight (Branch y l r) = Branch (f x y) (fLeft l) (fRight r)

tmap2Tree :: Test
tmap2Tree = "map2Tree" ~:
  map2Tree (+) (Branch 1 Leaf (Branch 2 Leaf Leaf)) (Branch 3 Leaf Leaf) ~?=
    Branch 4 Leaf Leaf

-- zipTree takes two trees and returns a tree of corresponding pairs. If
-- one input branch is smaller, excess elements of the longer branch are
-- discarded.
-- for example:  
--    zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf) (Branch True Leaf Leaf) returns 
--            (Branch (1,True) Leaf Leaf)

-- To use foldTree, you'll need to think about this one in
-- the same way as part (d).

zipTree :: Tree a -> Tree b -> Tree (a,b)
-- zipTree = map2Tree (,)
zipTree = foldTree (const Leaf) f'
  where
    f' _ _ _ Leaf = Leaf
    f' x fLeft fRight (Branch y l r) = Branch (x, y) (fLeft l) (fRight r)

tzipTree :: Test
tzipTree = "zipTree" ~:
  zipTree (Branch 1 (Branch 2 Leaf Leaf) Leaf) (Branch True Leaf Leaf) ~?=
    Branch (1,True) Leaf Leaf
----------------------------------------------------------------------

-- Smaller test play
play' :: SimpleXML
play' = Element "PLAY" [
  Element "TITLE" [PCDATA "TITLE OF THE PLAY"],
  Element "PERSONAE" [
    Element "PERSONA" [PCDATA "PERSON1"],
    Element "PERSONA" [PCDATA "PERSON2"]
  ],
  Element "ACT" [
    Element "TITLE" [PCDATA "TITLE OF FIRST ACT"],
    Element "SCENE" [
      Element "TITLE" [PCDATA "TITLE OF FIRST SCENE"],
      Element "SPEECH" [
        Element "SPEAKER" [PCDATA  "PERSON1"],
        Element "LINE" [PCDATA "LINE1"],
        Element "LINE" [PCDATA "LINE2"]
      ],
      Element "SPEECH" [
        Element "SPEAKER" [PCDATA  "PERSON2"],
        Element "LINE" [PCDATA "LINE3"],
        Element "LINE" [PCDATA "LINE4"]
      ]
    ],
    Element "SCENE" [
      Element "TITLE" [PCDATA "TITLE OF SECOND SCENE"],
      Element "SPEECH" [
        Element "SPEAKER" [PCDATA  "PERSON3"],
        Element "LINE" [PCDATA "LINE1"]
      ]
    ]
  ],
  Element "ACT" [
    Element "TITLE" [PCDATA "ACT 2"],
    Element "SCENE" [
      Element "TITLE" [PCDATA "ACT 2 Scene 1"],
      Element "SPEECH" [
        Element "SPEAKER" [PCDATA  "p1"],
        Element "LINE" [PCDATA "l1"]
      ]
    ]
  ]]

html' :: SimpleXML
html' = Element "html" [
  Element "body" [
    Element "h1" [PCDATA "TITLE OF THE PLAY"],
    Element "h2" [PCDATA "Dramatis Personae"],
    PCDATA "PERSON1", Element "br" [],
    PCDATA "PERSON2", Element "br" [],

    Element "h2" [PCDATA "TITLE OF THE FIRST ACT"],
    Element "h3" [PCDATA "TITLE OF THE FIRST SCENE"],
    Element "b" [PCDATA "PERSON1"], Element "br" [],
    PCDATA "LINE1", Element "br" [],
    PCDATA "LINE2", Element "br" [],
    Element "b" [PCDATA "PERSON2"], Element "br" [],
    PCDATA "LINE3", Element "br" [],
    PCDATA "LINE4", Element "br" [],
    Element "h3" [PCDATA "TITLE OF THE SECOND SCENE"],
    Element "b" [PCDATA "PERSON3"], Element "br" [],
    PCDATA "LINE1", Element "br" [],

    Element "h2" [PCDATA "ACT 2"],
    Element "h3" [PCDATA "ACT 2 Scene 1"],
    Element "b" [PCDATA "p1"], Element "br" [],
    PCDATA "l1", Element "br" [],
  ]
]

formatPlay :: SimpleXML -> SimpleXML
formatPlay = id

firstDiff :: Eq a => [a] -> [a] -> Maybe ([a],[a])
firstDiff [] [] = Nothing
firstDiff (c:cs) (d:ds) 
    | c==d = firstDiff cs ds 
    | otherwise = Just (c:cs, d:ds)
firstDiff cs ds = Just (cs,ds)

-- | Test the two files character by character, to determine whether
-- they match.
testResults :: String -> String -> IO ()
testResults file1 file2 = do 
  f1 <- readFile file1
  f2 <- readFile file2
  case firstDiff f1 f2 of
    Nothing -> return ()
    Just (cs,ds) -> assertFailure msg where
      msg  = "Results differ: '" ++ take 20 cs ++ 
            "' vs '" ++ take 20 ds

testXML :: Test
testXML = TestCase $ do 
  writeFile "dream.html" (xml2string (formatPlay play))
  testResults "dream.html" "sample.html"

