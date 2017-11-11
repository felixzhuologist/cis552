Monads III
==========

> {-# LANGUAGE NoImplicitPrelude #-}
> module Monads3 where
> import Prelude hiding (getLine, putStrLn, sequence, (>>), getLine)

> import State
> import Control.Monad (liftM, ap)

> import Data.Map (Map)
> import qualified Data.Map as Map

Recap from last time
====================

Last time we talked about "Store transformers", i.e. functions that take a
store and return a result and a new store.

> type Store = Int
> newtype ST2 a = S { apply :: Store -> (a, Store) }

We observed that the store transformer type is a Monad (and
Functor/Applicative):

> instance Functor ST2 where
>   fmap  = liftM
> instance Applicative ST2 where
>   pure  = return
>   (<*>) = ap
> instance Monad ST2 where
>   -- return :: a -> ST2 a
>   return x   = S (\ s -> (x,s))
>   -- (>>=)  :: ST2 a -> (a -> ST2 b) -> ST2 b
>   st >>= f   = S (\s -> let (x,s') = apply st s in apply (f x) s')


And put it to use "labelling" trees with fresh integers.

> data Tree a = Leaf a | Branch (Tree a) (Tree a)
>   deriving (Eq, Show)

> tree :: Tree Char
> tree =  Branch (Branch (Leaf 'a') (Leaf 'b')) (Leaf 'c')


> fresh :: ST2 Int  --- Store -> (Int, Store)
> fresh = S $ \s -> (s, s+1)

> mlabel            :: Tree a -> ST2 (Tree (a,Int))
> mlabel (Leaf x)       =  do 
>   s <- fresh 
>   return (Leaf (x, s))
> mlabel (Branch t1 t2) = do
>   t1' <- mlabel t1
>   t2' <- mlabel t2 
>   return (Branch t1' t2')

> label  :: Tree a -> Tree (a, Int)
> label t = fst (apply (mlabel t) 0)

For example, `label tree` gives the following result:

~~~~~{.haskell}
ghci> label tree
Branch (Branch (Leaf ('a', 0)) (Leaf ('b',1))) (Leaf ('c', 2))
~~~~~


Random Numbers
==============
See [RandomGen](RandomGen.html)


The IO Monad
============
Recall that interactive programs in Haskell are written using the
type `IO a` of "actions" that return a result of type `a`, but may
also perform some input/output.  A number of primitives are
provided for building values of this type, including:

~~~~~{.haskell}
return  :: a -> IO a
(>>=)   :: IO a -> (a -> IO b) -> IO b
getChar :: IO Char
putChar :: Char -> IO ()
~~~~~

The presence of `return` and `>>=` means that we can treat `IO` as a
monad, and hence (as we've seen) that the `do` notation can be used to
write interactive programs.  For example, the action that reads a
string of characters from the keyboard can be defined as follows:

> getLine :: IO String
> getLine = getChar >>= (\c -> 
>           if c == '\n' then return [] 
>           else getLine >>= (\cs -> return (c:cs)))


It is interesting to note that the `IO` monad can be viewed as a
special case of the State monad, in which the internal state is
a suitable representation of the "state of the world":

~~~~~{.haskell}
   type World = ...

   type IO a  = World -> (a,World)
~~~~~

That is, an action can be viewed as a function that takes the current
state of the world as its argument, and produces a value and a
modified world as its result, in which the modified world reflects any
input/output performed by the action.  In reality, Haskell systems
such as GHC implement actions in a more efficient manner, but for the
purposes of understanding the behavior of actions, the above
interpretation can be useful.


Monads As Programmable Semicolon
--------------------------------

It is sometimes useful to sequence two monadic expressions,
but discard the result value produced by the first:

      f (3);
      g (3);


> (>>) :: Monad m => m a -> m b -> m b
> mx >> my = mx >>= \ _ -> my


For example, in the state monad the `>>` operator is just normal
sequential composition, written as `;` in most languages. Without
using layout for the `do` notation, sequencing is a semicolon too.

> hello :: IO ()
> hello = putChar 'H' >> putChar 'e' >> putChar 'l' >> putChar 'l' >> putChar 'o'

> hi = do
>       putChar 'H'
>       putChar 'i'
>       return ()


Indeed, in Haskell the entire `do` notation with or without `;` is 
just [syntactic sugar][4] for `>>=` and `>>`. For this reason, we can 
legitimately say that Haskell has a [*programmable semicolon*][5].


Other topics
------------

The subject of monads is a large one, and we have only scratched the
surface here.  If you are interested in finding out more, you might
like to look at sections 3 and 7 of the following article, which
concerns the monadic nature of [functional parsers][3].  For a more
in-depth exploration of the IO monad, see Simon Peyton Jones'
excellent article on the ["awkward squad"][2].

[1]: http://en.wikipedia.org/wiki/Gofer_(software) "Gofer Language"
[2]: http://research.microsoft.com/Users/simonpj/papers/marktoberdorf/ "Awkward Squad"
[3]: http://www.cs.nott.ac.uk/~gmh/monparsing.pdf "Functional Parsers"
[4]: http://book.realworldhaskell.org/read/monads.html#monads.do
[5]: http://donsbot.wordpress.com/2007/03/10/practical-haskell-shell-scripting-with-error-handling-and-privilege-separation/
