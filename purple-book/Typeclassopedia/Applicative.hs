{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Data.Functor
--import Control.Applicative
import Data.List
import Data.Function
import Data.Foldable
import GHC.Maybe
import Data.Either
import GHC.Base(Applicative,liftA2,pure)
class Functor f => Applicative' f where
    pure' :: a -> f a 
    infixl 4 <*>, *> 
    (<*>) :: f (a -> b) -> f a -> f b -- this is normal functor
    (*> ) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) <*> a2
    (<*) :: f a -> f b  -> f a      
    (<*) = flip (*>) 
newtype ZipList a = ZipList { getZipList :: [a]}
instance Functor ZipList where
    fmap :: (a -> b ) -> ZipList a -> ZipList b
    fmap g (ZipList x) = ZipList (fmap g x)
instance  Applicative' ZipList where
    pure' :: a -> ZipList a 
    pure' x = ZipList [x]
    -- pure'  = getZipList  ?
    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (ZipList gs) <*> (ZipList xs)  = ZipList (zipWith ($) gs xs)
-- implementation of Either
instance Applicative' (Either e) where
    pure':: a -> Either e a
    pure' x = Right x
    (<*>) :: (Either e (a -> b)) -> (Either e a) -> (Either e b)
    (<*>) _ (Left x) = (Left x)
    --(<*>) (Left f) (Right y) =  (Right  y)
    (<*>) (Right f) (Right y) = (Right (f y))
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
-- instance Applicative (Either' e) where 
--     pure' :: a -> (Either' e a) 
--     pure' x = Right' x
--     (<*>):: (Either' e (a -> b)) -> (Either' e a) -> (Either' e b)
--     (<*>) (Right' f) (Right' x) = (Right' (f x))
--     (<*>) _  (Left' x)  = (Left' x)
instance Applicative' Maybe where
    pure' :: a -> Maybe a
    pure' a = Just a
    (<*>) :: Maybe ( a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just a <*> Just b = Just (a b)
 
sequenceAL :: GHC.Base.Applicative f => [f a] -> f [a]
sequenceAL  [] = pure []    
-- sequenceAL (x:xs) = pure' (:)  <*> x <*> sequenceAL xs 

sequenceAL (xs) = foldr (  liftA2 (:))   (pure []) (xs) 
-- alternate definition of applicative

-- implement pure and <*> in terms of unit and (**)
class Functor f => Monoidal f where
    unit :: f ()
    (**) :: f a -> f b -> f (a,b)
    newpure:: a -> f a
    newpure x = (**) unit x   
    --(<*>) :: f (a -> b) -> f a -> f b -- to avoid multple definition 
    app:: f (a -> b) -> f a -> f b -- to avoid multple definition 
    app f g =  fmap (uncarry $) f ** g -- was implementing just f**g, then took help to understand uncarry


-- Implentation of unit and ** using pure and <*>
unit' = pure' ()
fs newapp xs = pure' (,) <*> fs <*> xs -- impleentation of ** using pure and <*>

--3 . Tricky) Prove that given your implementations from the first exercise, 
-- the usual Applicative laws and the Monoidal laws stated above are equivalent.
-- unit ** v = pure (,) <*> unit <*> v
--           = pure (,) <*> pure () <*> v
--           = pure ((),) <*> v
--           = pure ((),v)  === v
-- v ** unit = pure (,) <*> v <*> unit
--           = pure (,) <*> v <*> pure ()
--           = pure (v,) <*> pure ()
--           = pure (v,()) === v
-- u ** (v ** w) = pure (,) <*> u <*> (pure (,) <*> v <*> w)
--               = pure (,) <*> pure $ (,) <*> u <*> v <*> w
--               = pure (,) <*> (u ** v) <*> w
--               = (u ** v) <*> w


-- Implementing the List data type in functor and applicative
data List a = Cons a (List a) | Nil
append:: List a -> List a -> List a
append Nil xs = xs
append (Cons x ys) xs = (Cons x (append ys xs))
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
    pure' :: a -> List a
    pure' x = Cons x (Nil)
    (<*>):: List (a -> b) -> List a -> List b
    (<*>) _ Nil = Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) xs = append (fmap f xs ) (fs <*> xs)


-- prove that pure f <*> x = pure (flip ($)) <*> x <*> pure f

-- pure (flip ($)) <*> x <*> pure f                  =
-- (pure (flip ($)) <*> x) <*> pure f                = -- associativity
-- pure (\x -> x f) <*> (pure (flip ($)) <*> x)      = -- Interchange
-- pure ($ f) <*> (pure (flip ($)) <*> x)            = ($ f) == (\x -> x f)
-- pure (.) <*> pure ($ f) <*> pure (flip ($)) <*> x = --composition
-- pure ((.) ($ f)) <*> pure (flip ($)) <*> x        = -- Homomorphism
-- pure ((.) ($ f) (flip ($))) <*> x                 = -- Homomorphism
-- pure (($ f) . flip ($)) <*> x                     = -- pre to in
-- pure f <*> x                                      = -- 
