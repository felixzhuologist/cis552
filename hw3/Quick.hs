{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults -fno-warn-orphans #-} 
{-# LANGUAGE FlexibleInstances, FlexibleContexts #-}


module Quick where

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import Data.List as List
import Data.Maybe as Maybe

import Test.QuickCheck
import Test.HUnit (Test (TestList), (~:), (~?=), runTestTT)
import Control.Monad (liftM, liftM2)
import System.Random (Random)



------------------------------------------------------------------------------
-- QuickCheck properties for lists

prop_const' :: Eq a => a -> a -> Bool
prop_const' a b = const a b == a 

-- *Main> quickCheck (prop_const :: Char -> Char -> Bool)

data Undefined
instance Testable Undefined where
  property = error "Unimplemented property"

prop_const :: Eq a => (a -> a -> a) -> a -> a -> Bool
prop_const const' a b = const' a b == a



constBug :: a -> a -> a
constBug _ b = b -- Oops: this returns the *second* argument, not the first.

prop_minimum :: Ord a => ([a] -> a) -> NonEmptyList a -> Bool
prop_minimum minimum' (NonEmpty xs) = all (>= m) xs && elem m xs
  where m = minimum' xs

minimumBug :: Ord a => [a] -> a
minimumBug = head

newtype SmallNonNeg a = SmallNonNeg a deriving (Eq, Ord, Show, Read)
 
instance (Num a, Random a, Arbitrary a) => Arbitrary (SmallNonNeg a) where
    arbitrary = liftM SmallNonNeg $ choose (0, 100)
    shrink (SmallNonNeg x) = map SmallNonNeg $ shrink x

prop_replicate :: Eq a => (Int -> a -> [a]) -> SmallNonNeg Int -> a -> Bool
prop_replicate replicate' (SmallNonNeg n) x = length xs == n && all (==x) xs
  where xs = replicate' n x

replicateBug :: Int -> a -> [a]
replicateBug _ x = [x, x]

prop_group_1 :: Eq a => ([a] -> [[a]]) -> [a] -> Bool
prop_group_1 group' xs = concat (group' xs) == xs

prop_group_2 :: Eq a => ([a] -> [[a]]) -> [a] -> Bool
prop_group_2 group' = all sameAndNE . group'
  where sameAndNE [] = False
        sameAndNE (x:xs) = all (==x) xs

groupBug :: Eq a => [a] -> [[a]]
groupBug (x:y:xs) = [x, y] : (groupBug xs)
groupBug [x] = []
groupBug _ = []

prop_reverse_1 :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_reverse_1 reverse' xs = all (flip elem (reverse' xs)) xs

prop_reverse_2 :: Eq a => ([a] -> [a]) -> [a] -> Bool
prop_reverse_2 reverse' xs = (reverse' (reverse' xs)) == xs

reverseBug_1 :: [a] -> [a]
reverseBug_1 [] = []
reverseBug_1 (x:xs) = reverse xs

reverseBug_2 :: [a] -> [a]
reverseBug_2 [] = []
reverseBug_2 (x:xs) = xs ++ [x]

listPropertiesMain :: IO ()
listPropertiesMain = do
  let qcName name prop = do
        putStr $ name ++ ": "
        quickCheck prop
  
  putStrLn "The following tests should all succeed:"
  qcName "const"     $ prop_const     (const     :: Char -> Char -> Char)
  qcName "minimum"   $ prop_minimum   (minimum   :: String -> Char)
  qcName "replicate" $ prop_replicate (replicate :: Int -> Char -> String)
  qcName "group_1"   $ prop_group_1   (group     :: String -> [String])
  qcName "group_2"   $ prop_group_2   (group     :: String -> [String])
  qcName "reverse_1" $ prop_reverse_1 (reverse   :: String -> String)
  qcName "reverse_2" $ prop_reverse_2 (reverse   :: String -> String)

  putStrLn ""

  putStrLn "The following tests should all fail:"
  qcName "const"     $ prop_const     (constBug     :: Char -> Char -> Char)
  qcName "minimum"   $ prop_minimum   (minimumBug   :: String -> Char)
  qcName "replicate" $ prop_replicate (replicateBug :: Int -> Char -> String)
  qcName "group_1"   $ prop_group_1   (groupBug     :: String -> [String])
  qcName "group_2"   $ prop_group_2   (groupBug     :: String -> [String])
  qcName "reverse_1" $ prop_reverse_1 (reverseBug_1 :: String -> String)
  qcName "reverse_2" $ prop_reverse_2 (reverseBug_2 :: String -> String)

------------------------------------------------------------------------------
-- Using QuickCheck to debug a SAT solver

--------------------------------------------------------------------------- 
runUnitTestsMain :: IO ()
runUnitTestsMain = do
  _ <- runTestTT $ TestList [ tCombinations,
                              tMakeValuations ]
  return ()

-- Basic types

-- | An expression in CNF (conjunctive normal form) is a conjunction
-- of clauses
type CNF = [ Clause ]
 
-- | A clause is a disjunction of a number of literals
data Clause = Clause [ Lit ] deriving (Eq, Ord, Show)
 
-- | A literal is either a positive or a negative variable
data Lit = Lit Bool Var deriving (Eq, Ord, Show)

-- | A variable is just a character
data Var = Var Char
  deriving (Eq, Ord, Show)

-- A few variables for test cases
vA, vB, vC, vD :: Var
vA = Var 'A'
vB = Var 'B'
vC = Var 'C'
vD = Var 'D'

-------------------------------------------------------------------------

-- | Extract the literals from a clause
lits :: Clause -> [Lit]
lits (Clause l) = l

-- | Extract the variable from a literal
var :: Lit -> Var
var (Lit _ x) = x
 
-- | Is the literal positive?
isPos :: Lit -> Bool
isPos (Lit b _) = b

-- | Determine the set of variables that appear in a formula
vars :: CNF -> Set Var
vars p = Set.unions $ map dVars p where
  dVars (Clause l) = Set.unions $ map (Set.singleton . var) l

instance Enum Var where
  toEnum i         = Var (toEnum (i + fromEnum 'A'))
  fromEnum (Var v) = fromEnum v - fromEnum 'A'

allVars :: [ Var ]
allVars = [vA .. ]

-------------------------------------------------------------------------

genVar      :: Int -> Gen Var
genVar    n = elements (take (abs n + 1) allVars)
 
genLit      :: Int -> Gen Lit
genLit    n = liftM2 Lit arbitrary (genVar n)
 
genClause   :: Int -> Gen Clause
genClause n = liftM Clause (listOf (genLit n))
 
genCNF      :: Int -> Gen CNF
genCNF     n = listOf (genClause n)

defaultNumVariables :: Int
defaultNumVariables = 5

instance Arbitrary Var where
  arbitrary = genVar defaultNumVariables
  shrink v | v == vA   = []
           | otherwise = [ vA .. pred v ]

instance Arbitrary Lit where
  arbitrary = genLit defaultNumVariables
  shrink (Lit b v) = map (flip Lit v) (shrink b) ++
                     map (Lit b) (shrink v)

instance Arbitrary Clause where
   arbitrary = genClause defaultNumVariables
   shrink (Clause l) = [Clause l' | l' <- shrink l]



---------------------------------------------------------------------
-- Satifiable and unsatisfiable formulae

exampleFormula :: CNF
exampleFormula = [Clause [Lit True vA, Lit True vB, Lit True vC],
                  Clause [Lit False vA],
                  Clause [Lit False vB, Lit True vC]]

unSatFormula :: CNF
unSatFormula = [Clause [Lit True vA],
                Clause [Lit False vA]]

-- | Assignments of values to (some) variables
type Valuation = Map Var Bool

emptyValuation :: Valuation
emptyValuation = Map.empty

fromList :: [(Var,Bool)] -> Valuation
fromList = Map.fromList

exampleValuation :: Valuation
exampleValuation = Map.fromList [(vA, False), (vB, True), (vC, True)]

litSatisfied :: Valuation -> Lit -> Bool
litSatisfied a (Lit b v) = Map.member v a && (b == a Map.! v)

satisfiedBy :: CNF -> Valuation -> Bool
satisfiedBy p a = all (any (litSatisfied a) . lits) p

prop_satisfiedBy :: Bool
prop_satisfiedBy = exampleFormula `satisfiedBy` exampleValuation

extend :: Var -> Bool -> Valuation -> Valuation
extend = Map.insert

value :: Var -> Valuation -> Maybe Bool
value = Map.lookup

---------------------------------------------------------------------------
-- Simple SAT Solver

type Solver = CNF -> Maybe Valuation

-- | Return all possible lists which consist of exactly one element from each 
-- sublist in the input. Assumes none of the sublists are empty.
combinations :: [[a]] -> [[a]]
combinations [] = []
combinations [x] = map (:[]) x
combinations (x:xs) = concat $ map f (combinations xs)
  where f recResult = map (:recResult) x

tCombinations :: Test
tCombinations = "combinations" ~:
  TestList [Set.fromList (combinations [[3, 4]]) ~?= Set.fromList ([[3], [4]]),
            Set.fromList (combinations [[1, 2], [3, 4]]) ~?= Set.fromList ([[1, 3], [1, 4], [2, 3], [2, 4]])]

makeValuations :: Set Var -> [Valuation]
makeValuations v = map fromList $ combinations $ map possibilities $ Set.toList v
  where possibilities v' = [(v', True), (v', False)]

tMakeValuations :: Test
tMakeValuations = "makeValuations" ~:
  TestList [makeValuations (Set.singleton (Var 'a')) ~?= [fromList [(Var 'a', True)],
                                                          fromList [(Var 'a', False)]]]

prop_makeValuations :: CNF -> Bool
prop_makeValuations p = length valuations == 2 ^ Set.size ss
                     && allElementsDistinct valuations where
   valuations = makeValuations ss
   ss = vars p

allElementsDistinct :: Eq a => [a] -> Bool
allElementsDistinct []     = True
allElementsDistinct (x:xs) = notElem x xs &&
                             allElementsDistinct xs

sat0 :: Solver
sat0 = undefined

prop_satResultSound :: Solver -> Int -> Property
prop_satResultSound solver i = 
  forAll (genCNF i) $ \p -> case solver p of
                               Just a  -> p `satisfiedBy` a
                               Nothing -> True

unsatisfiable :: CNF -> Bool
unsatisfiable p = all (\a -> not (p `satisfiedBy` a))
  (makeValuations (vars p))

prop_satResult :: Solver -> CNF -> Bool
prop_satResult solver p = case solver p of
                             Just a  -> p `satisfiedBy` a
                             Nothing -> unsatisfiable p

---------------------------------------------------------------------------
-- Instantiation

instantiate :: CNF -> Var -> Bool -> CNF
instantiate = undefined

prop_instantiate :: CNF -> Var -> Bool
prop_instantiate = undefined


sat1 :: Solver
sat1 = sat where
  sat = undefined

prop_sat1 :: CNF -> Bool
prop_sat1 s = isJust (sat1 s) == isJust (sat0 s)

--------------------------------------------------------------------------- 
-- Unit propagation

simplifyUnitClause :: CNF -> Maybe (CNF, Var, Bool)

-- 1) If (simplifyUnitClause s) returns Nothing, then there 
--    are no remaining unit clauses in s.
-- 2) If it returns (Just s'), then s' is satisfiable iff s is.
prop_simplifyUnitClause :: CNF -> Bool
prop_simplifyUnitClause = undefined

unitClauses :: CNF -> [Lit]
unitClauses = undefined

simplifyUnitClause = undefined

sat2 :: Solver
sat2 = sat where
  sat = undefined

prop_sat2 :: CNF -> Bool
prop_sat2 s = isJust (sat2 s) == isJust (sat0 s)

--------------------------------------------------------------------------- 
-- Pure literal elimination

simplifyPureLiteral :: CNF -> Maybe (CNF, Var, Bool)

-- 1) If (simplifyPureLiteral s) returns Nothing, then there 
--    are no remaining pure literals in s
-- 2) If it returns (Just s'), then s' is satisfiable iff s is
prop_simplifyPureLiteral :: CNF -> Bool
prop_simplifyPureLiteral = undefined



pureLiterals :: CNF -> [(Var,Bool)]
pureLiterals = undefined

simplifyPureLiteral = undefined

-- The final DPLL algorithm:
dpll :: Solver
dpll = sat where
  sat = undefined

prop_dpll :: CNF -> Bool
prop_dpll s = isJust (dpll s) == isJust (sat0 s)

------------------------------------------------------------------------------
-- Using QC as a SAT solver

instance Arbitrary (Map Var Bool) where
  arbitrary = undefined
  shrink = undefined

prop_isSatisfiable :: CNF -> Property
prop_isSatisfiable = undefined
 


------------------------------------------------------------------------------
-- All the tests in one convenient place:

main :: IO ()
main = quickCheck $    prop_satisfiedBy
                  .&&. prop_satResultSound sat0 defaultNumVariables
                  .&&. prop_satResult      sat0 
                  .&&. prop_instantiate
                  .&&. prop_sat1
                  .&&. prop_satResultSound sat1 
                  .&&. prop_satResult      sat1 
                  .&&. prop_simplifyUnitClause
                  .&&. prop_sat2
                  .&&. prop_satResultSound sat2 
                  .&&. prop_satResult      sat2 
                  .&&. prop_simplifyPureLiteral
                  .&&. prop_dpll
                  .&&. prop_satResultSound dpll 
                  .&&. prop_satResult      dpll 
