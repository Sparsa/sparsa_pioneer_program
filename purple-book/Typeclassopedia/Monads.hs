{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
import Control.Applicative
import Data.List
import Data.Function
import Data.Functor
class Applicative m => Monad' m where
    return' :: a -> m a -- same as pure
    (>>=) :: m a -> (a -> m b) -> m b 
    (>>) :: m a ->  m b -> m b
    m >> n = m >>= \_ -> n  
-- Instance of Monad of []
instance Monad' [] where
    return' :: a -> [a] 
    return' x = [x]
    (>>=) :: [a] -> (a -> [b]) -> [b]
    (x:xs) >>= g = (g x) ++ (xs >>= g)

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
{- Laws of Monads
return a >>= k  =  k a
m >>= return    =  m
m >>= (\x -> k x >>= h)  =  (m >>= k) >>= h
-}
-- Proof that the implementation of bind (>>=) for pair is lawful
-- first law
-- return x >>= k 
-- => Pair x x >>= k 
-- => k x 
-- second law
-- m >>= return = m 
-- Pair x x >>= return 
-- => return x 
-- => Pair x x 
-- third law:
-- m >>= (\x -> k x >>= h) 
-- => Pair x x >>= (\x -> k x >>= h)
-- => (\x ->k x >>= h) Pair x x 
-- => k (Pair x x) >>= h
-- => (Pair x x >>= k) >>= h -- from the definition of >>= for pair. Hence proved.
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
    (Var h) <*> (Node xs) = fmap h (Node xs)
  --  (Node sh) <*> (Node xs) = (fmap ($) (Node sh))


    
instance Functor f => Monad' (Free f) where
    return' :: a -> Free f a
    return' x = (Var x)
    (>>=) :: (Free f a) -> (a -> Free f b) -> (Free f b)
    (Var x) >>= g = (g x)
    Node (xs) >>= g = Node (fmap (>>= g) xs) 

-- Monad Intuition exercises
-- Implement bind (>>=) with fmap and join
class Applicative m => Monad'' m where
    return :: a -> m a
    join :: m (m a) -> (m a)
    bind :: m a -> (a -> m b) -> m b
    bind xs f = join (fmap f xs) -- definition of bind in terms of join and fmap
    fmap' :: (a -> b) -> m a -> m b
    fmap' f xs = bind xs (return.f) -- definition of fmap using bind and return
    join' :: m (m a) -> (m a) 
    join' xs = bind xs id -- definition of join using bind 
    

--Implemnt Join and fmap interms of bind
-- fmap :: Monad' m => (a -> b) -> m a -> m b
-- fmap f xs = 
join'' :: (Monad' m) => m (m a) -> m a
join'' m = m >>= id


fmap'' :: (Monad' m) => (a -> b) -> m a -> m b
fmap'' f m = m >>= (return . f)


-- Exercises

--     Implement join :: M (N (M (N a))) -> M (N a), given distrib :: N (M a) -> M (N a) and assuming M and N are instances of Monad.

-- Assuming M and N monads, so they will have their version of fmap_N and fmap_M respectively.
-- Also they should have their version of Join_M and Join_N respectively.

--joinM :: Monad' m => Monad' n => m(n(m(n a))) -> m (n a)
-- distrib ::Monad' m => Monad' n => n (m a) -> m (n a)
-- -- fmap_M distrib M(N(M(N a)))  = M(M(N(N a))) -- this swaps the M and N in the second and third level
-- -- join_M fmap_M distrib M(N(M(N a))) = M (N (N a)) -- then applying Join will remove the first M from the top layer.
--joinM x =  fmap join ( join (fmap distrib x)) -- now use fmap and join to remove the N from the second layer.
-- -- Is this correct?

{- Given the definition g >=> h = \x -> g x >>= h, prove the equivalence of the above laws and the usual monad laws. -}
-- return >=> g 
-- => \x -> return x >>= g -- from the definition above 
-- => return >>= g -- \x -> return x == return
-- => g -- from first law of monads \qed

-- g >=> return 
-- => \x -> g x >>= return -- applying the definition above
-- \x -> g x -- applying 2nd law of monad 
-- g -- from the lambda calculus \qed

-- (g >=> h)  >=> k 
-- => 