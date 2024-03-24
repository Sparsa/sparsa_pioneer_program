{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
import Data.Functor
import Control.Applicative
import Data.List ( map )
import Data.Function
import GHC.Types
import GHC.Classes
import GHC.Num 
import Data.Monoid (Endo(..) )
import Data.List(concat)
import GHC.Show
import GHC.Read
import GHC.Maybe 
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
--foldMap' :: Monoid m => ( a -> m) -> t a -> m 
-- foldMap' g =   fold (\x acc -> mconcat acc (g x)) mempty-- we need a monoid identity, and op
-- implementing foldr using fold
--foldr'' ::Monoid m => (a ->b->b) -> b -> t a -> b 
-- foldr'' f e xs = fold 

-- Question 3.
-- implement foldMap in terms of foldr
foldMap'' g xs = foldr f mempty xs where -- note that the function f is defined as (a -> b -> b) so you need to define it with respect with the g.
    f y z = mappend (g y) z -- here we are doing that. g takes an argument a and converts it to m mappend appends two monads so the f function works
    -- defined as a -> m -> m basically it converts a to a monad and then it appends those using mappned.

-- Question 4.
{- an endomorphism is commonly used to represent functions that operate within a certain type and return values of the same type. 
This is particularly useful in the context of function composition and monoids.
-}
-- Implement foldr in terms of foldMap (hint: use the Endo monoid).
foldr'''' :: (Foldable t, Monoid (Endo a)) => (b -> a -> a) -> a -> t b -> a
foldr'''' f z xs = appEndo (foldMap (Endo . f) xs) z

-- Question 5:
--- What is the type of foldMap . foldMap? Or foldMap . foldMap . foldMap, etc.? What do they do?
-- (a -> m) -> t1 (t2 a) -> m , (a -> m) -> t1 (t2 (t3 a)) -> m
-- this means that multiple application of foldMapt is actually unwrap multiple layers of foldables and apply the function on the inner type

-- Exercieses regarding derived folds
-- 1. Implement toList :: Foldable f => f a -> [a] in terms of either foldr or foldMap.
toList' :: Foldable f => f a -> [a]
toList'  = foldr (:) [] 

toList'' :: Foldable f => f a -> [a]
toList'' = foldMap (\x -> [x]) 
prop :: forall (a :: Type) . Eq a => [a] -> Bool
prop = (==) <$> toList' <*> id
prop2 :: forall (a :: Type) . Eq a => [a] -> Bool
prop2 = (==) <$> toList'' <*> id
-- 2. How one could implement the generic version of foldr in terms of toList, 
-- assuming we had only the list-specific foldrList :: (a -> b -> b) -> b -> [a] -> b.
-- foldr''' :: Foldable f => ( a -> b -> b) -> b -> f a -> b
-- foldr''' f x xs = foldrList f x (toList xs)  -- basically use the toList to convert the foldable to a list and then apply the foldr on list

-- Pick some of the following functions to implement:
-- concat, concatMap, and, or, any, all, sum, product, maximum(By), minimum(By), elem, notElem, and find. 
-- Figure out how they generalize to Foldable and come up with elegant implementations using fold or foldMap 
-- along with appropriate Monoid instances.
concat :: (Foldable t) => t [a] -> [a]
concat xs = foldMap id xs

concatMap :: (Foldable t) => (a -> [b]) -> t a -> [b]
concatMap f xs = foldMap f xs

newtype All = All { getAll :: Bool } deriving (Eq, Show, Ord, Read)
-- took help to understand that I need to define this All and Any monoids,
-- Please let me know if any other way possible
instance Monoid All where
         mempty                  = All True
         mappend (All x) (All y) = All (x && y)

newtype Any = Any { getAny :: Bool } deriving (Eq, Show, Ord, Read)
instance Monoid Main.Any where
         mempty               = Any False
         mappend (Any True) _ = Any True
         mappend _ (Any True) = Any True
         mappend _ _          = Any False

and :: (Foldable t) => t Bool -> Bool
and = getAll . foldMap All 

or :: (Foldable t) => t Bool -> Bool
or = getAny . foldMap Any 

any :: (Foldable t) => (a -> Bool) -> t a -> Bool
any f = getAny . foldMap (Any . f) 

all :: (Foldable t) => (a -> Bool) -> t a -> Bool
all f = getAll . foldMap (All . f) 

newtype Sum = Sum { getSum :: Int } deriving (Eq, Show, Ord, Read)
instance Monoid Sum where
         mempty = Sum 0
         mappend (Sum x) (Sum y) = Sum (x + y)

newtype Product = Product { getProd :: Int } deriving (Eq, Show, Ord, Read)
instance Monoid Product where
         mempty = Product 1 
         mappend (Product x) (Product y) = Product (x * y)

sum' :: (Foldable t) => t Int -> Int
sum' = getSum . foldMap Sum

product' :: (Foldable t) => t Int -> Int
product' = getProd . foldMap Product



elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = Main.any (==x) 

notElem :: (Foldable t, Eq a) => a -> t a -> Bool
notElem x = Main.all (/=x) 

newtype First a = First { getFirst :: Maybe a } deriving (Eq, Ord, Read, Show)
instance Monoid (First a) where
  mempty = First Nothing
  mappend (First Nothing) r = r
  mappend l _ = l

find :: (Foldable t) => (a -> Bool) -> t a -> Maybe a
find f = getFirst . foldMap (\x -> First (if (f x) then (Just x) else Nothing))
-- Implement traverse_ and sequenceA_ with each other
-- traverse_ :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f ()
-- sequenceA_ :: (Applicative f, Foldable t) => t (f a) -> f ()

traverse_ :: (Applicative f, Foldable t) => (a -> f b) -> t a -> f ()
traverse_ g = foldr ((*>). g) (pure ())
--sequenceA_ (fmap g xs)

sequenceA_ :: (Applicative f, Foldable t) => t  (f a) -> f()
sequenceA_ xs = traverse_ id xs 
