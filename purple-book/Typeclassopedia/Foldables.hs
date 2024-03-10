{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
import Data.Functor
import Control.Applicative
import Data.List ( map )
import Data.Function
import GHC.Types
import GHC.Classes
import GHC.Num 
class Monoid a where
    mempty :: a
    mappend :: a -> a -> a 
    mconcat :: [a] -> a  
    --mconcat = foldr mappend mempty -- notice that here mempty is acting as the accumulator and the operations is append. 
class Foldable t where
    fold :: Monoid m => t m -> m
    foldMap :: Monoid m => ( a -> m) -> t a -> m
    foldr :: (a -> b -> b ) -> b -> t a -> b 
    foldr' :: (a ->b->b) -> b -> t a -> b
    foldl :: (b -> a -> b) -> b -> t a -> b 
    foldl' :: (b -> a -> b) -> b -> t a -> b    
    foldr1 :: (a -> a -> a ) -> t a -> a  
    foldl1 :: (a -> a -> a ) -> t a -> a 
    toList :: t a -> [a]  
    null :: t a -> Bool 
    length :: t a -> Int 
    elem :: Eq a => a -> t a -> Bool 
    maximum :: Ord a => t a -> a 
    minimum :: Ord a => t a -> a 
    sum :: Num a => t a -> a 
    product :: Num a => t a -> a
    -- if you implement any one of the foldr or foldMap then you will have the default implementation of the rest
instance Foldable [] where
    foldMap :: Monoid m => ( a -> m) -> [a] -> m
    foldMap g = mconcat . map g

data Tree a = Empty | Leaf a | Node ( Tree a ) a ( Tree a)

instance Foldable Tree where
    foldMap :: Monoid m => ( a -> m) -> Tree a -> m
    foldMap f Empty  =  mempty
    foldMap f (Leaf x)  =  f x 
    foldMap f (Node xs x ys)  = mappend  (mappend  ( foldMap f xs) (f x) ) (foldMap f ys) 

-- Implement fold in terms of foldMap
fold' :: Monoid m => Foldable t => t m -> m 
fold' = foldMap id
-- What you need in order to implement foldMap in terms of fold?
foldMap' :: Monoid m => Foldable t => ( a -> m) -> t a -> m 
-- foldMap' g xs =   fold (fmap g xs)  we need a map.
-- implement foldMap in terms of foldr
foldMap' g xs = foldr f mempty xs where -- note that the function f is defined as (a -> b -> b) so you need to define it with respect with the g.
    f y z = mappend (g y) z -- here we are doing that. g takes an argument a and converts it to m mappend appends two monads so the f function works
    -- defined as a -> m -> m basically it converts a to a monad and then it appends those using mappned.

-- Implement foldr in terms of foldMap (hint: use the Endo monoid).
-- foldr'' :: Foldable t=> Monad m =>  ( a -> b -> b) -> b -> t a -> b
-- foldr'' f x xs  =  foldMap g (reverse xs) where
--     g y = 
-- Implement foldMap interms of foldr
-- foldr' :: Foldable t => ( a-> b -> b) -> b -> t a -> b
-- foldr' f x xs =  foldMap (f x) xs
--- What is the type of foldMap . foldMap? Or foldMap . foldMap . foldMap, etc.? What do they do?
-- (a -> m) -> t1 (t2 a) -> m , (a -> m) -> t1 (t2 (t3 a)) -> m
-- this means that multiple application of foldMapt is actually unwrap multiple layers of foldables and apply the function on the inner type

-- Exercieses regarding derived folds
toList' :: Foldable f => f a -> [a]
toList' xs = foldr (++) [] fa 
-- toList' xs = foldMap (++) xs


-- How one could implement the generic version of foldr in terms of toList, 
-- assuming we had only the list-specific foldrList :: (a -> b -> b) -> b -> [a] -> b.
-- foldr''' :: Foldable f => ( a -> b -> b) -> b -> f a -> b
-- foldr''' f x xs = foldrList f x (toList xs)  -- basically use the toList to convert the foldable to a list and then apply the foldr on list


-- 
-- Implement traverse_ and sequenceA_ with each other
-- traverse_ :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f ()
-- sequenceA_ :: (Applicative f, Foldable t) => t (f a) -> f ()

traverse_ :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f ()
traverse_ g xs = sequenceA_ (fmap g xs)

sequenceA_ :: (Applicative f, Foldable t) => t  (f a) -> f()
sequenceA_ xs = traverse_ id xs 


