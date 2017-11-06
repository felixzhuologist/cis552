{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables, FlexibleInstances #-}

module AVL (Set(..),AVL(..),
            avlEmpty,avlElements,avlMember,avlInsert,avlDelete,
            t1,t2,t3,bad1,bad2,bad3,main,rebalance,height,bf,
            setProperties,prop_empty,prop_elements,prop_insert1,
            prop_insert2,prop_delete1,prop_delete2,prop_delete3,
            avlProperties,prop_bst,prop_ht,prop_balance) where
import Prelude hiding (zipWith,zipWith3)
import Test.QuickCheck hiding (elements)
import Data.List (sort, nub)

class Set s where
   empty    :: s a
   member   :: Ord a => a -> s a -> Bool
   insert   :: Ord a => a -> s a -> s a
   elements :: s a -> [a]
   delete   :: Ord a => a -> s a -> s a

instance Set AVL where
   empty    = avlEmpty
   member   = avlMember
   insert   = avlInsert
   elements = avlElements
   delete   = avlDelete

-- 1 

-- height of empty tree is 0
prop_empty :: Bool
prop_empty = height E == 0

-- elements are sorted and unique
prop_elements :: AVL Int -> Bool
prop_elements x = (sort $ nub xs) == xs where xs = elements x

-- inserted element is a member
prop_insert1 :: Int -> AVL Int -> Bool
prop_insert1 x t = member x $ insert x t

-- no elements are removed on insert
prop_insert2 :: Int -> AVL Int -> Bool
prop_insert2 x t = all (\y -> member y (insert x t)) (elements t)

-- tree is balanced after insert
prop_insert3 :: Int -> AVL Int -> Bool
prop_insert3 x t = prop_balance $ insert x t

-- elements are no longer members after being deleted
prop_delete1 :: AVL Int -> Bool
prop_delete1 t = all (\x -> not (member x (delete x t))) (elements t)

-- deleting maintains balance
prop_delete2 :: AVL Int -> Bool
prop_delete2 t = all (\x -> prop_balance (delete x t)) (elements t)

-- deleting x from tree without x has no effect
prop_delete3 :: AVL Int -> Int -> Property
prop_delete3 t x = not (x `elem` elements t) ==> (delete x t == t)

setProperties :: Property
setProperties = 
  counterexample "empty"   prop_empty    .&&.
  counterexample "elts"    prop_elements .&&. 
  counterexample "insert1" prop_insert1  .&&.
  counterexample "insert2" prop_insert2  .&&.
  counterexample "insert3" prop_insert3  .&&.
  counterexample "delete1" prop_delete1  .&&.
  counterexample "delete2" prop_delete2  .&&.
  counterexample "delete3" prop_delete3 

data AVL e = E           -- empty tree
           | N           -- non-empty tree
               Int       -- cached height of the tree
               (AVL e)   -- left subtree
               e         -- value
               (AVL e)   -- right subtree
  deriving Show

-- | Access the height of the tree
height :: AVL e -> Int
height E = 0
height (N h _ _ _) = h

-- | Calculate the balance factor of a node
bf :: AVL e -> Int
bf E = 0
bf (N _ l _ r) = height l - height r

-- | The tree is a binary search tree
prop_bst :: AVL Int -> Bool
prop_bst = prop_bst' (minBound :: Int) (maxBound :: Int)

prop_bst' :: Int -> Int -> AVL Int -> Bool
prop_bst' lb ub (N _ l x r) = x > lb && x < ub && (prop_bst' lb x l) && (prop_bst' x ub r)
prop_bst' _ _ E = True

-- | The height at each node is correctly calculated. 
prop_ht :: AVL Int -> Bool
prop_ht E = True
prop_ht t = snd $ prop_ht' t

-- | return both height and whether the height at each node in subtree is correct
prop_ht' :: AVL Int -> (Int, Bool)
prop_ht' E = (1, True)
prop_ht' (N h l _ r) = (actualHeight, prop)
  where 
    actualHeight = max heightLeft heightRight
    prop = actualHeight == h && propLeft && propRight
    (heightLeft, propLeft) = prop_ht' l
    (heightRight, propRight) = prop_ht' r

-- | The balance factor at each node is between -1 and +1.  
prop_balance :: AVL Int -> Bool
prop_balance E = True
prop_balance t@(N _ l _ r) = elem (bf t) [-1, 0, 1] && prop_balance l && prop_balance r

avlProperties :: Property
avlProperties = 
  counterexample "bst"     prop_bst .&&.
  counterexample "height"  prop_ht .&&.
  counterexample "balance" prop_balance

instance (Eq a) => Eq (AVL a) where
  (==) E E = True
  (==) (N _ l1 x1 r1) (N _ l2 x2 r2) =
    x1 == x2 && l1 == l2 && r1 == r2
  (==) _ _ = False

instance (Ord e, Arbitrary e) => Arbitrary (AVL e) where
    arbitrary = g where
      g :: forall a. (Ord a, Arbitrary a) => Gen (RBT a)
      g = liftM (foldr insert empty) (arbitrary :: Gen [a])

-- | an empty AVL tree
avlEmpty :: AVL e
avlEmpty = E

-- | list the elements in the tree, in order
avlElements :: AVL e -> [e]
avlElements E = []
avlElements (N _ l x r) = avlElements l ++ [x] ++ avlElements r

-- | Determine if an element is contained within the tree
avlMember :: Ord e => e -> AVL e -> Bool
avlMember x E = False
avlMember x (N _ l y r)
  | x < y     = avlMember x l 
  | x > y     = avlMember x r
  | otherwise = True

t0 :: AVL Int
t0 = E

t1 :: AVL Int
t1 = N 1 E 1 E

t2 :: AVL Int
t2 = N 2 (N 1 E 2 E) 5 E

t3 :: AVL Int
t3 = N 3 (N 2 (N 1 E 3 E) (N 1 E 7 E)) 10 (N 2 (N 1 E 11 E) 13 E)

-- unbalanced
bad1 :: AVL Int
bad1 = N 3 (N 2 (N 1 E 0 E) 1 E) 2 E

-- wrong height
bad2 :: AVL Int
bad2 = N 5 E 1 E

-- not BST
bad3 :: AVL Int
bad3 = N 2 E 3 (N 1 E 0 E)


-- 4 rotation cases:
-- outside (right)

--    10                     15
--   /  \                   /  \
--  5    15                10   17
--      /  \              / \   / 
--     11   17           5  11 16
--         /
--        16

-- inside (right)

--    10                     10                        12
--   /  \                   /  \                      /  \
--  5    15                5   12                    10   15 
--      /  \                  /  \                  /  \  / \
--     12   17               11  15                5  11 12  17
--    /                            \
--   11                            17

-- | Rotate an AVL tree 
-- take tree whose root node has balance factor -2 or +2 and rearrange it to
-- an equivalent tree with -1/0/+1 balance factors
rebalance :: (Ord e) => AVL e -> AVL e
rebalance t@(N _ l x r) =
  | bf t == -2 && bf r == -1 = outsideRight t
  | bf t == -2 && bf r == 1 = insideRight t
  | bf t == 2 && bf l == -1 = insideLeft t
  | bf t == 2 && bf l == 1 = outsideLeft t
  | otherwise = t

outsideRight (N _ t1 A (N _ t2 B (N _ t3 C t4))) = N _ (N _ t1 A t2) B (N _ t3 C t4)
insideRight (N _ t1 A (N _ (N _ t2 C t3) t4)) = N _ (N _ t1 A t2) C (N _ t3 B t4)

-- | Insert a new element into a tree, returning a new tree
avlInsert :: (Ord e) => e -> AVL e -> AVL e
avlInsert y t@(N h l x r) =
  | x < y = rebalance $ (N insLeftHeight insLeft x r)
  | x > y = rebalance $ (N insRightHeight l x insRight)
  | otherwise = t
  where
    insLeftHeight = max h $ height insLeft
    insRightHeight = max h $ height insRight
    insLeft = avlInsert y l
    insRight = avlInsert y r
avlInsert y _ = N 1 E y E

-- | Delete the provided element from the tree
avlDelete :: Ord e => e -> AVL e -> AVL e
avlDelete y t@(N h l x r) = undefined




main :: IO ()
main = return ()

