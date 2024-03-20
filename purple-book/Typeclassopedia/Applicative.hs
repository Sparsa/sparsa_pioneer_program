{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Functor
--import Control.Applicative
import Data.List
import Data.Function
import Data.Foldable
import GHC.Maybe
class Functor f => Applicative' f where
    pure' :: a -> f a 
    infixl 4 <*>, *> 
    (<*>) :: f (a -> b) -> f a -> f b -- this is normal functor
    (*> ) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2

--    (<*) :: f a -> f b  -> f a      
--    (<*) = liftA2 const
--
newtype ZipList a = ZipList { getZipList :: [a]}
instance Functor ZipList where
    fmap :: (a -> b ) -> ZipList a -> ZipList b
    fmap g (ZipList x) = ZipList (fmap g x)
instance  Applicative' ZipList where
    pure' :: a -> ZipList a 
    --pure' x = ZipList [x]
    pure' x = getZipList x
    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (ZipList gs) <*> (ZipList xs)  = ZipList (zipWith ($) gs xs)

instance Applicative' [] where
    pure' :: a -> [a]
    pure' x = [x]
    (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs , x <- xs]
-- instance of identity
data Identity a = Id a
instance Functor Identity where
    fmap :: (a->b) -> (Identity a) -> (Identity b)
    fmap g (Id x) = Id (g x)
-- Instance of Identity
instance Applicative' Identity where
    pure' :: a -> Identity a
    pure' x = Id x 
    (<*>) :: Identity ( a -> b) -> (Identity a) -> (Identity b )
    Id f <*> Id x = Id (f x)
-- instance of Either
data Either' a b = Left' a | Right' b
instance Functor (Either' e) where
    fmap :: (a->b) -> Either' e a -> Either' e b
    fmap _ (Left' x) = Left' x
    fmap g (Right' x) = Right' (g x)
instance Applicative' (Either' e) where 
    pure' :: a -> (Either' e a) 
    pure' x = Right' x
    (<*>):: (Either' e (a -> b)) -> (Either' e a) -> (Either' e b)
    (<*>) (Right' f) (Right' x) = (Right' (f x))
    (<*>) _  (Left' x)  = (Left' x)
instance Applicative' Maybe where
    pure' :: a -> Maybe a
    pure' a = Just a
    (<*>) :: Maybe ( a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just a <*> Just b = Just (a b)
 
sequenceAL :: Applicative' f => [f a] -> f [a]
sequenceAL  [] = pure' []    
--sequenceAL (x:xs) = pure' (:)  <*> x <*> sequenceAL xs 

sequenceAL (x:xs) = foldr (\ x -> (<*>) (pure' (:) <*> x)) (pure' []) xs
-- alternate definition of applicative

class Functor f => Monoidal f where
    unit :: f ()
    (**) :: f a -> f b -> f (a,b)

-- Implementing the List data type in functor and applicative
data List a = Cons a (List a) | Nil
instance Functor List  where
    fmap :: (a -> b) -> (List a) -> (List b)
    fmap _ Nil = Nil
    fmap f (Cons x xs) = (Cons (f x) (fmap f xs))
-- instance Applicative' List  where
--     (<*>) :: List ( a -> b) -> List a -> List b 
--     (<*>) _ Nil =  Nil
--     (<*>) (Cons g gs) (Cons x xs) = (Cons (g x) ((<*>) gs xs))
--     pure' :: a -> List a
--     pure' x = Cons x (Nil)
instance Applicative' List where
    (<*>):: List (a -> b) -> List a -> List b
    (<*>) _ Nil = Nil
