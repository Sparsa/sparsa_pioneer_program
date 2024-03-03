{-# LANGUAGE InstanceSigs #-}
class Functor f => Applicative' f where
    pure' :: a -> f a 
    infixl 4 <*>, *> 
    (<*>) :: f (a -> b) -> f a -> f b -- this is normal functor
    (*> ) :: f a -> f b -> f b
    a1 *> a2 = (id <$ a1) Main.<*> a2

--    (<*) :: f a -> f b  -> f a      
--    (<*) = liftA2 const
--
newtype ZipList a = ZipList { getZipList :: [a]}
instance Functor ZipList where
    fmap :: (a -> b ) -> ZipList a -> ZipList b
    fmap g (ZipList x) = ZipList (fmap g x)
instance  Applicative' ZipList where
    pure' :: a -> ZipList a 
    pure' x = ZipList [x]
    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    (ZipList gs) <*> (ZipList xs)  = ZipList (zipWith ($) gs xs)

instance Applicative' [] where
    pure' :: a -> [a]
    pure' x = [x]
    (<*>) :: [a -> b] -> [a] -> [b]
    gs <*> xs = [g x | g <- gs , x <- xs]


instance Applicative' Maybe where
    pure' :: a -> Maybe a
    pure' a = Just a
    (<*>) :: Maybe ( a -> b) -> Maybe a -> Maybe b
    Nothing <*> _ = Nothing
    _ <*> Nothing = Nothing
    Just a <*> Just b = Just (a b)

sequenceAL :: Applicative' f => [f a] -> f [a]
sequenceAL  [] = pure' []    
sequenceAL (x:xs) = pure' (:)  Main.<*> x Main.<*> sequenceAL xs 

-- alternate definition of applicative

class Functor f => Monoidal f where
    unit :: f ()
    (**) :: f a -> f b -> f (a,b)


