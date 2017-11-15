{-# OPTIONS -Wall -fno-warn-unused-binds -fno-warn-unused-matches -fwarn-tabs -fno-warn-type-defaults #-}
{-# LANGUAGE NoImplicitPrelude, FlexibleInstances, ScopedTypeVariables #-}

module Main where

import Prelude hiding (mapM,sequence)

import Data.List()
import Data.Maybe (isJust)
import Data.Set (Set, member)
import qualified Data.Set as Set (singleton, fromList)
 
import Control.Applicative(Alternative(..))
import Control.Monad (ap)

import Test.HUnit hiding (State)
import Test.QuickCheck
import Test.QuickCheck.Function

main :: IO ()
main = return ()

-------------------------------------------------------------------------
-- (a) Define a monadic generalization of map 

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f xs = sequence $ map f xs
 
testMapM :: Test
testMapM = undefined

-- (b) Define a monadic generalization of foldl

foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f acc (x:xs) = (f acc x) >>= (\x' -> foldM f x' xs)
foldM _ acc [] = return acc

testFoldM :: Test
testFoldM = undefined

-- (c) Define a generalization of monadic sequencing that evaluates
-- each action in a list from left to right, collecting the results
-- in a list.

sequence :: Monad m => [m a] -> m [a]
sequence = foldr (liftM2 (:)) (return [])

testSequence :: Test 
testSequence = undefined

-- (d) Define the "fish operator", a variant of composition

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
f >=> g = (\x -> f x >>= g)
 
testKleisli :: Test
testKleisli = undefined

-- (e) Define the 'join' operator, which removes one level of
-- monadic structure, projecting its bound argument into the outer level. 

join :: (Monad m) => m (m a) -> m a
join mmx = mmx >>= id
 
testJoin :: Test 
testJoin = undefined

-- (f) Define the 'liftM' function

liftM   :: (Monad m) => (a -> b) -> m a -> m b
liftM f = (\mx -> mx >>= (\x -> return $ f x))

testLiftM :: Test
testLiftM = undefined 

liftM2  :: (Monad m) => (a -> b -> r) -> m a -> m b -> m r
liftM2 f mx my = do
  x <- mx
  y <- my
  return $ f x y

testLiftM2 :: Test
testLiftM2 = undefined  

-------------------------------------------------------------------------

data AList a = Nil | Single a | Append (AList a) (AList a) deriving (Show, Eq)

-- (a)

instance Functor AList where
   -- fmap :: (a -> b) -> AList a -> AList b
   fmap f Nil = Nil
   fmap f (Single x) = Single $ f x
   fmap f (Append xs ys) = Append (fmap f xs) (fmap f ys)

instance Applicative AList where
   pure   = return   -- use monadic functions for this instance
   (<*>)  = ap

instance Monad AList where
   -- return :: a -> AList a
   return = Single
   -- (AList a) -> (a -> AList b) -> AList b 
   Nil >>= _ = Nil
   (Single x) >>= f = f x
   (Append xs ys) >>= f = Append (xs >>= f) (ys >>= f) 

prop_FMapId :: (Eq (f a), Functor f) => f a -> Bool
prop_FMapId x = fmap id x == id x

prop_FMapComp :: (Eq (f c), Functor f) => Fun b c -> Fun a b -> f a -> Bool
prop_FMapComp (Fun _ f) (Fun _ g) x =
   fmap (f . g) x == (fmap f . fmap g) x

prop_LeftUnit :: (Eq (m b), Monad m) => a -> Fun a (m b) -> Bool
prop_LeftUnit x (Fun _ f) = 
   (return x >>= f) == f x

prop_RightUnit :: (Eq (m b), Monad m) => m b -> Bool
prop_RightUnit m = 
   (m >>= return) == m

prop_Assoc :: (Eq (m c), Monad m) =>
    m a -> Fun a (m b) -> Fun b (m c) -> Bool
prop_Assoc m (Fun _ f) (Fun _ g) =
   ((m >>= f) >>= g) == (m >>= \x -> f x >>= g)

prop_FunctorMonad :: (Eq (m b), Functor m, Monad m) => m a -> Fun a b -> Bool
prop_FunctorMonad x (Fun _ f) = fmap f x == (x >>= return . f)

instance Arbitrary a => Arbitrary (AList a) where
  arbitrary = oneof [return Nil, liftM Single arbitrary, liftM2 Append arbitrary arbitrary] 
  shrink (Append xs ys) = [xs, ys]
  shrink _ = []

qc1 :: IO ()
qc1 = quickCheck
         (prop_FMapId  :: AList Int -> Bool)

qc2 :: IO ()
qc2 = quickCheck
         (prop_FMapComp :: Fun Int Int -> Fun Int Int -> AList Int -> Bool)

qc3 :: IO ()
qc3 = quickCheck
         (prop_LeftUnit  :: Int -> Fun Int (AList Int) -> Bool)

qc4 :: IO ()
qc4 = quickCheck (prop_RightUnit :: AList Int -> Bool)

qc5 :: IO ()
qc5 = quickCheck
           (prop_Assoc :: AList Int -> Fun Int (AList Int) -> Fun Int (AList Int) -> Bool)

qc6 :: IO ()
qc6 = quickCheck
           (prop_FunctorMonad :: AList Int -> Fun Int (AList Int) -> Bool)

-- test that monad is implemented the same way for lists and alists using 
-- toList to compare results

prop_MapEq :: (Eq b) => AList a -> Fun a b -> Bool
prop_MapEq x (Fun _ f) = (fmap f (toList x)) == (toList $ fmap f x)

prop_ReturnEq :: (Eq a) => a -> Bool
prop_ReturnEq x = (return x) == (toList $ return x)

prop_BindEq :: (Eq b) => AList a -> Fun a (AList b) -> Bool
prop_BindEq x (Fun _ f) = (toList x >>= (toList . f)) == (toList $ x >>= f)

qc7 :: IO ()
qc7 = quickCheck
            (prop_MapEq :: AList Int -> Fun Int Int -> Bool)

qc8 :: IO ()
qc8 = quickCheck
            (prop_ReturnEq :: Int -> Bool)

qc9 :: IO ()
qc9 = quickCheck
            (prop_BindEq :: AList Int -> Fun Int (AList Int) -> Bool)

qcAList :: IO()
qcAList = qc1 >> qc2 >> qc3 >> qc4 >> qc5 >> qc6 >> qc7 >> qc8 >> qc9

-- (b) : come up with implementation that type checks but fails above properties

{- Invalid instance of Functor and Monad:

instance Functor AList where
   fmap f s = undefined
instance Monad AList where
   return = undefined
   (>>=)  = undefined
-}

-- (c)

-- | access the first element of the list, if there is one.
first :: AList a -> Maybe a 
first Nil = Nothing
first (Single x) = Just x
first (Append xs ys) = first xs <|> first ys

-- | access the last element of the list, if there is one
final :: AList a -> Maybe a
final Nil = Nothing
final (Single x) = Just x
final (Append xs ys) = final ys <|> final xs
 
-- (d)

instance Alternative AList where
  empty = Nil
  (<|>) = Append

search :: (Alternative m) => (a -> Bool) -> AList a -> m a
search _ Nil = empty
search p (Single x) = if p x then pure x else empty
search p (Append xs ys) = search p xs <|> search p ys

-- N.b.: Do not change the type signatures or definitions of `findAny`,
-- `findAll`, or -- `prune`.  They must be exactly as we give them here.  

findAny :: (a -> Bool) -> AList a -> Maybe a 
findAny = search

findAll :: (a -> Bool) -> AList a -> [a]
findAll = search

prune :: (a -> Bool) -> AList a -> AList a
prune = search

testSearch :: Test
testSearch = TestList [ "search1" ~: findAny (>3)    seq1 ~=? Just 4, 
                        "search2" ~: findAll (>3)    seq1 ~=? [4,5],
                        "search3" ~: prune (>3) seq1 ~=? 
                         Append (Append Nil Nil) 
                           (Append (Single 4) (Single 5))] 

seq1 :: AList Int
seq1 = Append (Append Nil (Single 3)) (Append (Single 4) (Single 5))

-- (e) 

fromList :: [a] -> AList a
fromList = foldr (Append . Single) Nil 

toList :: AList a -> [a]
toList Nil            = []
toList (Single x)     = [x]
toList (Append s1 s2) = toList s1 ++ toList s2

showAsList :: Show a => AList a -> String
showAsList = show . toList

exAList :: AList Int
exAList = Append (Append (Append (Append (Single 0) 
                                        (Single 1)) 
                                 (Single 2))
                        (Single 3)) 
                 (Single 4)

testToList :: Test
testToList = toList exAList ~?= [0,1,2,3,4]

toListFast :: AList a -> [a]
toListFast = undefined

prop_toList :: Eq a => AList a -> Bool
prop_toList l = toListFast l == toList l

-------------------------------------------------------------------------

data RegExp = Char (Set Char)      -- single literal character
            | Alt RegExp RegExp    -- r1 | r2   (alternation)
            | Seq RegExp RegExp    -- r1 r2     (concatenation)
            | Star RegExp          -- r*        (Kleene star)
            | Empty                -- Îµ, accepts empty string
            | Void                 -- âˆ…, always fails 
            | Mark RegExp          -- (for marked subexpressions, see (b) below)
  deriving Show

char :: Char -> RegExp
char = Char . Set.singleton

chars :: String -> RegExp
chars = Char . Set.fromList

lower, upper, letter, digit, punc, white, anyc, anyc':: RegExp
lower  = chars ['a' .. 'z']
upper  = chars ['A' .. 'Z']
digit  = chars ['0' .. '9']
punc   = chars "<>!/.*()?@"
white  = chars " \n\r\t"

anyc'  = lower `Alt` upper `Alt` digit `Alt` punc `Alt` white

anyc = chars $ ['a' .. 'z']
               ++ ['A' .. 'Z']
               ++ ['0' .. '9']
               ++ "<>!/.*()?@"
               ++ "\n\r\t"

letter = chars $ ['A' .. 'Z'] ++ ['a' .. 'z']

word :: String -> RegExp
word = foldr (Seq . char) Empty 

cis552 :: RegExp
cis552 = word "cis552"

boldHtml :: RegExp
boldHtml = word "<b>" `Seq` Star anyc `Seq`  word "</b>"

plus :: RegExp -> RegExp
plus pat = pat `Seq` Star pat

-- (a)

-- all decompositions of a string into two different pieces
--     split "abc" == [("","abc"),("a","bc"),("ab","c"),("abc","")]
split :: [a] -> [([a], [a])]
split s = map (\n -> (take n s, drop n s)) [0..length s]

-- all decompositions of a string into multi-part (nonempty) pieces
-- parts "abc" = [["abc"],["a","bc"], ["ab","c"], ["a","b","c"]]
parts :: [a] -> [[[a]]]
-- need single empty list in the case of (s, "") when mapping in recursive case
parts [] = [[]]
parts [c] = [[[c]]]
-- only "expand" recursively one of left/right so that you don't
-- get duplicate results
parts s = concatMap (\(l, r) -> map ([l]++) (parts r)) (tail $ split s)

accept :: RegExp -> String -> Bool
accept (Mark r)    s = accept r s
accept (Char cs) [c] = member c cs
accept (Alt r1 r2) s = (accept r1 s) || (accept r2 s)
accept (Seq r1 r2) s = any (\(l, r) -> (accept r1 l) && (accept r2 r)) (split s)
accept (Star r)   [] = True
accept (Star reg)    s = any (\(l, r) -> (accept reg l) && (accept (Star reg) r)) (split s)
accept Empty      [] = True
accept _ _ = False

testAccept :: Test
testAccept = TestList [ 
   not (accept Void "a") ~? "nothing is void",
   not (accept Void "") ~? "really, nothing is void",
   accept Empty "" ~? "accept Empty true", 
   not (accept Empty "a") ~? "not accept Empty",
   accept lower "a" ~? "accept lower",
   not (accept lower "A") ~? "not accept lower",
   accept boldHtml "<b>cis552</b>!</b>" ~? "cis552!",
   not (accept boldHtml "<b>cis552</b>!</b") ~? "no trailing" ]

-- (b)

-- | Mark a subexpression
-- this function just wraps the data constructor for now -- included for future 
-- extensibility
mark :: RegExp -> RegExp  
mark = Mark

boldHtmlPat :: RegExp
boldHtmlPat = word "<b>" `Seq` mark (Star anyc) `Seq` word "</b>"

namePat :: RegExp
namePat = mark (plus letter) `Seq` Star white `Seq` mark (plus letter) 

wordsPat :: RegExp
wordsPat = Star (mark (plus lower) `Seq` Star white)

testPat :: Test
testPat = TestList [
    patAccept boldHtmlPat "<b>cis552" ~?= Nothing,
    patAccept boldHtmlPat "<b>cis552!</b>" ~?= Just ["cis552!"],
    patAccept boldHtmlPat "<b>cis552</b>!</b>" ~?= Just ["cis552</b>!"],
    patAccept namePat "Haskell  Curry" ~?= Just ["Haskell", "Curry"], 
    patAccept wordsPat "a    b c   d e" ~?= Just ["a", "b", "c", "d", "e"]
  ]

patAccept :: RegExp -> String -> Maybe [String]
patAccept (Mark r)    s = if accept r s then Just [s] else Nothing
patAccept (Char cs) [c] = if member c cs then Just [] else Nothing
patAccept (Alt r1 r2) s = case (patAccept r1 s, patAccept r2 s) of
  (Just x, Just y) -> Just (x ++ y)
  (Just x, Nothing) -> Just x
  (Nothing, Just y) -> Just y
  _ -> Nothing
patAccept (Seq r1 r2) s = if any isJust matches then 
                            liftM concat $ sequence $ filter isJust matches else
                            Nothing
  where
    matches = map patAcceptSplit (split s)
    patAcceptSplit (l, r) = liftM2 (++) (patAccept r1 l) (patAccept r2 r)
patAccept (Star r)   [] = Just []
patAccept (Star reg)  s = if any isJust matches then 
                            liftM concat $ sequence $ filter isJust matches else
                            Nothing
  where
    matches = map patAcceptSplit (split s)
    patAcceptSplit (l, r) = liftM2 (++) (patAccept reg l) (patAccept (Star reg) r)
patAccept Empty      [] = Just []
patAccept _ _ = Nothing


-- (c)

match :: RegExp -> String -> Bool
match r s = nullable (foldl deriv r s)

-- | `nullable r` return `True` when `r` matches the empty string
nullable :: RegExp -> Bool
nullable Empty = True
nullable (Star _) = True
nullable (Mark r) = nullable r
nullable (Alt r1 r2) = nullable r1 || nullable r2
nullable (Seq r1 r2) = nullable r1 && nullable r2
nullable _ = False

-- |  Takes a regular expression `r` and a character `c`,
-- and computes a new regular expression that accepts word `w` if `cw` is
-- accepted by `r`.
deriv :: RegExp -> Char -> RegExp
deriv (Mark r) c = deriv r c
deriv (Char cs) c = if member c cs then Empty else Void
deriv (Alt r1 r2) c = Alt (deriv r1 c) (deriv r2 c)
deriv (Seq r1 r2) c = case deriv r1 c of
  Empty -> r2
  Void -> Void
  _ -> (Seq (deriv r1 c) r2)
deriv (Star r) c = if (match r [c]) then (Star r) else Empty
deriv Empty _ = Void
deriv _ _ = Void



