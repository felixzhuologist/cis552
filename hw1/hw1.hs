{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

{-# LANGUAGE NoImplicitPrelude #-}

module Main where
import Prelude hiding (all, reverse, takeWhile, zip, concat, concatMap)
import Test.HUnit 

main :: IO ()
main = do 
   _ <- runTestTT $ TestList [ testStyle,
                               testLists,
                               testBowlingKata,
                               testLcs ]
   return ()

--------------------------------------------------------------------------------

testStyle :: Test
testStyle = "testStyle" ~:
   TestList [ tabc , tarithmetic, treverse, tzap ]


abc :: Bool -> Bool -> Bool -> Bool
abc x y z = (x && z) || (x && y)


tabc :: Test
tabc = "abc" ~: TestList [abc True False True ~?= True, 
                          abc True False False ~?= False,
                          abc False True True ~?= False]


arithmetic :: ((Int, Int), Int) -> ((Int,Int), Int) -> (Int, Int, Int)
arithmetic ((a, b), c) ((d, e), f) =
       (((b*f) - (c*e)), ((c*d) - (a*f)), ((a*e)-(b*d)))
 

tarithmetic :: Test
tarithmetic = "arithmetic" ~:
   TestList[ arithmetic ((1,2),3) ((4,5),6) ~?= (-3,6,-3), 
             arithmetic ((3,2),1) ((4,5),6) ~?= (7,-14,7) ]


reverse :: [a] -> [a]
reverse = foldl (flip (:)) []
 

treverse :: Test
treverse = "reverse" ~: TestList [reverse [3,2,1] ~?= [1,2,3],
                                  reverse [1]     ~?= [1] ]

zap :: [a -> b] -> [a] -> [b]
zap = zipWith (\func arg -> (func arg))

tzap :: Test
tzap = "zap" ~:
  TestList [ zap [ (+1), \n -> n - 1, (+1) ]
                   ([3, 4, 5] :: [Int]) ~?= [4,3,6],
             zap [ null, not . null ] [ [], "a" ] ~?= [True, True],
             zap [] "a" ~?=  "",
             zap [not] [] ~?= []]

-------------------------------------------------------------------------------- 

testLists :: Test
testLists = "testLists" ~: TestList [tintersperse, tinvert, ttakeWhile, tfind, tall, tmap2, tzip, ttranspose, tconcat]

-- The intersperse function takes an element and a list 
-- and intersperses that element between the elements of the list. 
-- For example,
--    intersperse ',' "abcde" == "a,b,c,d,e"

tintersperse :: Test
tintersperse = "intersperse" ~: TestList [intersperse ',' "abcde" ~?= "a,b,c,d,e",
                                          intersperse 2 [1, 3]        ~?= [1, 2, 3],
                                          intersperse 3 [4]           ~?= [4]]

intersperse :: a -> [a] -> [a]
intersperse _ [x] = [x]
intersperse del (x:xs) = x : del : (intersperse del xs)
intersperse _ _  = []

-- invert lst returns a list with each pair reversed. 
-- for example:
--   invert [("a",1),("a",2)]     returns [(1,"a"),(2,"a")] 
--   invert ([] :: [(Int,Char)])  returns []

--   note, you need to add a type annotation to test invert with []
--    

tinvert :: Test
tinvert = "invert" ~: TestList [invert [("a", 1), ("a", 2)]  ~?= [(1, "a"), (2, "a")],
                                invert ([] :: [(Int, Char)]) ~?= []]

invert :: [(a, b)] -> [(b, a)]
invert = map (\(x, y) -> (y, x))

-- takeWhile, applied to a predicate p and a list xs, 
-- returns the longest prefix (possibly empty) of xs of elements 
-- that satisfy p:
-- For example, 
--     takeWhile (< 3) [1,2,3,4,1,2,3,4] == [1,2]
--     takeWhile (< 9) [1,2,3] == [1,2,3]
--     takeWhile (< 0) [1,2,3] == []

ttakeWhile :: Test
ttakeWhile = "takeWhile" ~: TestList [takeWhile (< 3) [1, 2, 3, 4, 1, 2, 3, 4] ~?= [1, 2],
                                      takeWhile (< 9) [1, 2, 3] ~?= [1, 2, 3],
                                      takeWhile (< 0) [1, 2, 3] ~?= []]

takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile _ [] = []
takeWhile f (x:xs)
  | f x       = x : (takeWhile f xs)
  | otherwise = []

-- find pred lst returns the first element of the list that 
-- satisfies the predicate. Because no element may do so, the 
-- answer is returned in a "Maybe".
-- for example: 
--     find odd [0,2,3,4] returns Just 3

tfind :: Test
tfind = "find" ~: TestList [find odd [0, 2, 3, 4]      ~?= Just 3,
                            find odd [0, 2, 4, 20, -2] ~?= Nothing,
                            find odd []                ~?= Nothing]
 
find :: (a -> Bool) -> [a] -> Maybe a
find _ [] = Nothing
find f (x:xs)
  | f x       = Just x
  | otherwise = find f xs

-- all pred lst returns False if any element of lst fails to satisfy
-- pred and True otherwise.
-- for example:
--    all odd [1,2,3] returns False

tall :: Test
tall = "all" ~: TestList [all odd [1, 2, 3]  ~?= False,
                          all odd [1, 3, -5] ~?= True,
                          all odd []         ~?= True]
 
all :: (a -> Bool) -> [a] -> Bool
all f = foldl (\result x -> result && (f x)) True

-- map2 f xs ys returns the list obtained by applying f to 
-- to each pair of corresponding elements of xs and ys. If 
-- one list is longer than the other, then the extra elements 
-- are ignored.
-- i.e. 
--   map2 f [x1, x2, ..., xn] [y1, y2, ..., yn, yn+1] 
--        returns [f x1 y1, f x2 y2, ..., f xn yn]
--
-- NOTE: map2 is called zipWith in the standard library.

tmap2 :: Test
tmap2 = "map2" ~: TestList [map2 (+) [1, 2, 3] [4, 5, 6]   ~?= [5, 7, 9],
                            map2 (&&) [True, False] [True] ~?= [True]]

map2 :: (a -> b -> c) -> [a] -> [b] -> [c]
map2 f xs ys = map (uncurry f) $ zip xs ys

-- zip takes two lists and returns a list of corresponding pairs. If
-- one input list is shorter, excess elements of the longer list are
-- discarded.
-- for example:  
--    zip [1,2] [True] returns [(1,True)]

tzip :: Test
tzip = "zip" ~: TestList [zip [1, 2] [True] ~?= [(1, True)]]

zip :: [a] -> [b] -> [(a, b)]
zip (x:xs) (y:ys) = (x, y) : zip xs ys
zip _ [] = []
zip [] _ = []

-- transpose  (WARNING: this one is tricky!)

-- The transpose function transposes the rows and columns of its argument. 
-- If the inner lists are not all the same length, then the extra elements
-- are ignored. Note, this is not the same behavior as the library version
-- of transpose.
 
-- for example:
--    transpose [[1,2,3],[4,5,6]] returns [[1,4],[2,5],[3,6]]
--    transpose  [[1,2],[3,4,5]] returns [[1,3],[2,4]]

ttranspose :: Test
ttranspose = "transpose" ~: TestList [transpose [[1, 2, 3], [4, 5, 6]] ~?= [[1, 4], [2, 5], [3, 6]],
                                      transpose [[1, 2], [3, 4, 5]]    ~?= [[1, 3], [2, 4]],
                                      transpose [[1, 2, 3]]            ~?= [[1], [2], [3]]]

transpose :: [[a]] -> [[a]]
transpose l
  | any null l = []
  | otherwise  = (map head l) : (transpose $ map tail l)

-- concat
 
-- The concatenation of all of the elements of a list of lists
-- for example:
--    concat [[1,2,3],[4,5,6],[7,8,9]] returns [1,2,3,4,5,6,7,8,9]

tconcat :: Test
tconcat = "concat" ~: TestList [concat [[1, 2, 3], [4, 5], [6]] ~?= [1, 2, 3, 4, 5, 6],
                                concat [[], [1]]                ~?= [1]]

concat :: [[a]] -> [a]
concat = foldl (++) []

-- concatMap
 
-- Map a function over all the elements of the list and concatenate the results.
-- for example:
--    concatMap (\x -> [x,x+1,x+2]) [1,2,3]  returns [1,2,3,2,3,4,3,4,5]
 
tconcatMap :: Test
tconcatMap = "concatMap" ~: TestList [concatMap (\x -> [x, x + 1, x + 2]) [1, 2, 3] ~?= [1, 2, 3, 2, 3, 4, 3, 4, 5]]

concatMap :: (a -> [b]) -> [a] -> [b]
concatMap f l = concat $ map f l

--------------------------------------------------------------------------------

bowlingTest0 :: ([Int] -> Int) -> Test
bowlingTest0 score = "all gutter balls" ~: 0 ~=? score (replicate 20 0)

score0 :: [ Int ] -> Int
score0 _ = 0

bowlingTest1 :: ([Int] -> Int) -> Test
bowlingTest1 score = 
   "allOnes" ~: 20 ~=? score (replicate 20 1)

score1 :: [ Int ] -> Int
score1 = sum

bowlingTest2 :: ([ Int ] -> Int) -> Test
bowlingTest2 score = "spare" ~: 20 ~=? score ([7, 3, 10] ++ (replicate 17 0))

score2 :: [ Int ] -> Int
score2 (x:y:z:xs) = case x + y of
  10 -> 10 + z + (score2 $ z:xs)
  _  -> x + y + (score2 $ z:xs)
score2 l = sum l

score2a :: [ Int ] -> Int
score2a = score where
   score = score2

bowlingTest3 :: ([ Int ] -> Int) -> Test
bowlingTest3 _ = "always fail" ~: assertFailure "add a test case"

score3 :: [ Int ] -> Int
score3 = score where
   score _ = 0

bowlingTest4 :: ([ Int ] -> Int) -> Test
bowlingTest4 score = "perfect game" ~: 300 ~=? score (replicate 12 10) 

score4 :: [ Int ] -> Int
score4 = score where
     score _ = 0

testBowlingKata :: Test
testBowlingKata = TestList (map checkOutput scores) where
  -- the five test cases, in order 
  bowlingTests  = [bowlingTest0, bowlingTest1, bowlingTest2, 
                   bowlingTest3, bowlingTest4]
 
  -- the names of the score functions, the functions themselves, 
  -- and the expected number of passing tests
  scores = zip3 ['0' ..] [score0, score1, score2a, score3, score4] [1..]
 
  -- a way to run a unit test without printing output 
  testSilently = performTest (\ _ _ -> return ()) 
                   (\ _ _ _ -> return) (\ _ _ _ -> return) ()
 
  -- run each bowling test on the given score function, making sure that 
  -- the expected number of tests pass.
  checkOutput (name, score, pass) = " Testing score" ++ [name] ~: do 
    (s0,_) <- testSilently 
       (TestList $ bowlingTests `zap` replicate 5 score)
    assert $ pass @=? cases s0 - (errors s0 + failures s0)

-------------------------------------------------------------------------------- 

lcs :: String -> String -> String 
lcs = error "unimplemented: lcs"

testLcs :: Test
testLcs = "Lcs" ~: TestList [ lcs "Advanced" "Advantaged" ~?= "Advaned",
    lcs "abcd" "acbd" ~?= "acd" ]



