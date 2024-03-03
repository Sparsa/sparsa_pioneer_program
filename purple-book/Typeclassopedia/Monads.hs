{-# LANGUAGE InstanceSigs #-}
class Applicative m => Monad' m where
    return' :: a -> m a -- same as pure
    (>>=) :: m a -> (a -> m b) -> m b 
    (>>) :: m a ->  m b -> m b
    m >> n = m Main.>>= \_ -> n  
-- Instance of Monad of []
instance Monad' [] where
    return' :: a -> [a] 
    return' x = [x]
    (>>=) :: [a] -> (a -> [b]) -> [b]
    (x:xs) >>= g = (g x) ++ (xs Main.>>= g)

-- Instance of Monad of ((->), e)
instance Monad' ((->) e) where
    return' :: a -> (e -> a)
    return' x = \y -> x  
    (>>=) :: (e -> a ) -> (a -> (e -> b)) -> (e -> b)
    g >>= f =  \y -> f ( g y) y 

-- Instance of Monad of Pair 
-- defined functor and applicative for pair to define monad
data Pair' a = Pair' a a
instance Functor Pair' where
    fmap :: (a->b) -> (Pair' a) -> (Pair' b)
    fmap g (Pair' x y) = Pair' (g x) (g y)
instance Applicative Pair' where
    pure :: a -> Pair' a
    pure x = Pair' x x 
    (<*>) :: (Pair' (a ->  b)) -> Pair' a -> Pair' b 
    (Pair' g h) <*> (Pair' x y)=  (Pair' (g x) (g y)) 
instance Monad' Pair' where
    return' :: a -> Pair' a
    return' x = Pair' x x
    (>>=) :: Pair' a -> ( a -> Pair' b) -> (Pair' b)
    Pair' x y >>= f = f x  
-- Implementation of monad of Free
data Free f a = Var a  
 | Node (f (Free f a)) 

instance Functor f => Functor (Free f)  where
    fmap :: (a -> b) -> (Free f a ) -> (Free f b)
    fmap g (Var x) = (Var (g x))
    fmap g (Node  xs) = Node (fmap (fmap g) xs) -- the first fmap is on f and the second fmap is on Free
instance Functor f => Applicative (Free f) where
    pure :: a -> (Free f a)
    pure x = (Var x)
    (<*>) :: (Free f (a -> b)) -> (Free f a) -> (Free f b)
    (Var h) <*> (Var x) = (Var (h x))
    

    
instance Functor f => Monad' (Free f) where
    return' :: a -> Free f a
    return' x = (Var x)
    (>>=) :: (Free f a) -> (a -> Free f b) -> (Free f b)
    (Var x) >>= g = (g x)
    Node (xs) >>= g = Node (fmap (Main.>>= g) xs) 


    
