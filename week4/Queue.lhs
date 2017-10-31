---
fulltitle: "Exercise: Purely Functional Queues"
date: October 11, 2017
---

> {-# LANGUAGE ScopedTypeVariables, AllowAmbiguousTypes #-}
> 
> {-# OPTIONS_GHC -fdefer-type-errors #-}
> module Queue where

> import Test.QuickCheck hiding (elements)
> 

1. Define an interface for a purely functional Queue (FIFO) using a type
class. It must (at least) have a way to add an element to the end of the queue
(enqueue) and remove the element at the beginning of the queue (dequeue).

> class Queue s where
>   enqueue  :: a -> s a -> s a
>   dequeue  :: s a -> s a
>   elements :: s a -> [a]


2. Define some properties that your queue should satisfy.

> prop_enqueue_1 :: Int -> [Int] -> Bool
> prop_enqueue_1 x q = elem x (elements $ enqueue x q)

> prop_enqueue_2 :: Int -> [Int] -> Bool
> prop_enqueue_2 x q = all (\y -> elem y $ elements $ enqueue x q) (elements q)

> prop_dequeue_1 :: [Int] -> Bool
> prop_dequeue_1 q = (length $ elements $ dequeue q) <= (length $ elements q)

3. Implement your interface.

> instance Queue [] where
>   enqueue = (:)
>   dequeue [] = []
>   dequeue q = init q
>   elements = id

4. Make an arbitrary instance.

>

5. Run your tests.

> main :: IO ()
> main = do
>   quickCheck $ prop_enqueue_1
>   quickCheck $ prop_enqueue_2
>   quickCheck $ prop_dequeue_1
