{-# LANGUAGE InstanceSigs #-}
-- define local functor
class NewFunctor f where
    fmap' :: (a->b) -> f a -> f b
-- Implementation of the functor instance of Either e and ((->) e)
-- Either e
data Either' a b = Left' a | Right' b
 
instance NewFunctor (Either' e) where
    fmap' :: (a->b) -> Either' e a -> Either' e b
    fmap' _ (Left' x) = Left' x
    fmap' g (Right' x) = Right' (g x)
instance NewFunctor ((->) e) where
    fmap' :: (a -> b) -> (e -> a) -> e -> b
    fmap' g f = g.f 
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

data ITree a = Leaf (Int -> a)
 | Node [ITree a]

instance NewFunctor ITree where
    fmap' :: (a -> b) -> ITree a -> ITree b
    fmap' _ (Node []) = Node []
    fmap' g (Leaf x) = Leaf (g . x)
    fmap' g (Node xs) = Node( map (fmap' g ) xs) 

-- Give an example of a type kind *-> * which can not be made instance of Functor

--The composition of two functor is also a functor.    
-- How to show that it is true using Haskell programming. (should follow from the law?)