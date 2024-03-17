{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
import GHC.Show
import GHC.Base (String,Int, (.), (++), map)

-- define local functor
class NewFunctor f where
    fmap' :: (a->b) -> f a -> f b
-- Implementation of the functor instance of Either e and ((->) e)
-- Either e
data Either' a b = Left' a | Right' b
data Maybe a = Nothing | Just a
data Identity a = Id a

instance NewFunctor Identity where
    fmap' :: (a->b) -> (Identity a) -> (Identity b)
    fmap' g (Id x) = Id (g x)

instance NewFunctor Maybe where
    fmap' :: (a->b) -> (Maybe a) -> (Maybe b)
    fmap' _ Nothing = Nothing
    fmap' g (Just x) = Just (g x)


-- Exercises from the Instances part
-- Implement Functor instances of Either e and ((->),e) 
instance NewFunctor (Either' e) where
    fmap' :: (a->b) -> Either' e a -> Either' e b
    fmap' _ (Left' x) = Left' x
    fmap' g (Right' x) = Right' (g x)
instance NewFunctor ((->) e) where
    fmap' :: (a -> b) -> (e -> a) -> e -> b
    fmap' g f = g.f 
-- Implement Functor instances for ((,) e) and of Pair 
instance NewFunctor ((,) e) where
    fmap' :: (a -> b) -> (e, a) -> (e, b)
    fmap' g (y,x) = (y,g x)

data Pair a = Pair a a 
instance (Show a) =>  Show (Pair a) where
    show :: Show a => Pair a -> String
    show (Pair i j) = "(" ++ show i++", " ++ show j ++ ")"
instance NewFunctor Pair where
    fmap' :: (a -> b) -> Pair a -> Pair b
    fmap' g (Pair x y) = Pair (g x) (g y)
-- Implement Functor Instance for the type ITree, defined as
data ITree a = Leaf (Int -> a)
 | Node [ITree a]

instance NewFunctor ITree where
    fmap' :: (a -> b) -> ITree a -> ITree b
    fmap' _ (Node []) = Node []
    fmap' g (Leaf x) = Leaf (g . x)
    fmap' g (Node xs) = Node( map (fmap' g ) xs) 

-- Give an example of a type kind *-> * which can not be made instance of Functor
type MaybeArrow b a = Maybe a -> Maybe b
{- note that MaybeArrow has a kind type *->* i.e., it takes one type
as input and one type as output. But as we know the functor implementation
(fmap) works on a single type argument, whereas here the datatype uses two different
argument types (a and b). Plus, functor deals with data structures, not functions. 
A functor takes a function and applies it to the data inside the structure.
It can not handle the function.
-}
--The composition of two functor is also a functor.    
-- How to show that it is true using Haskell programming. (should follow from the law?)
{-instance (NewFunctor f, g) => NewFunctor (f(g)) where
    fmap' :: (a -> b) -> f(g(a)) -> f(g(b))
    fmap' fun = fmap  (fmap fun) 

Think of a two dimentional list, the first functor will work on the
first dimention and the second will work on the second dimention
    
-}

-- Exercises for on the Laws of the functor

{-Although it is not possible for a Functor instance to satisfy the first 
Functor law but not the second (excluding undefined),
 the reverse is possible. Give an example of a (bogus) 
 Functor instance which satisfies the second law but not the first.
 -}

 -- Evil Functor instance
instance NewFunctor [] where
  fmap' :: (a -> b) -> [a] -> [b]
  fmap' _ [] = []
  fmap' g (x:xs) = g x : g x : fmap' g xs
{- This fails the first law of functors,
fmap' id [2,3,4]
= [2,2,3,3,4,4] =/= [2,3,4]
-}