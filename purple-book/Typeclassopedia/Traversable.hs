
{-# LANGUAGE NoImplicitPrelude #-}
import Data.Functor
import Control.Applicative
import Data.List ( map )
import Data.Either
import Data.Function
import GHC.Types
import GHC.Classes
import GHC.Num
import GHC.Base ( (++), foldr, Monad, Maybe(Nothing, Just) )
import Data.Foldable (Foldable)
import GHC.Show ( Show )
import Prelude (foldMap)
import GHC.Maybe(Maybe(..))
data Tree a = Empty | Leaf a | Node (Tree a) a ( Tree a) deriving (Show)
data BinaryTree a = BiLeaf (Maybe a) | BiNode (BinaryTree a) ( a) (BinaryTree a) deriving (Show)
-- is it the correct way to define a binary tree with two constructor?
t2 = Node (Leaf (Just 2)) (Just 4) (Node (Leaf (Just 5)) (Just 7) (Leaf (Just 8)))
t3 = Node (Leaf (Just 2)) (Just 4) (Node (Leaf (Just 5)) (Just 7) (Leaf Nothing))
listT = [t2,t3]
class (Functor t, Foldable t) => Traversable t where
  traverse  :: Applicative f => (a -> f b) -> t a -> f (t b)
  sequenceA :: Applicative f => t (f a) -> f (t a)
  mapM      ::       Monad m => (a -> m b) -> t a -> m (t b)
  sequence  ::       Monad m => t (m a) -> m (t a)

-- There are atleast two natural ways to turn a tree of lists into a list of trees
-- Ans we can apply sequenceA where t (the traversable) is the tree and f (the applicative) is the list
-- What is the second way?

-- Give a natural way to turn a list of trees in a tree of lists
-- first we create a function that convers individual tree elements
-- of the list to lists, and make the outer list to a tree
toListTree :: Tree a -> Tree [a]
toListTree Empty = Empty
toListTree (Leaf x) = Leaf [x]
toListTree (Node xs x ys) = Node (toListTree xs)  [x]  (toListTree ys)
toListTree' :: Functor Tree =>  Tree a -> Tree [a]
toListTree' = fmap (: [])


-- a function that concatenates two tree of lists.
treeConcat :: Tree [a] -> Tree [a] -> Tree [a]
treeConcat Empty Empty = Empty
treeConcat Empty (Leaf x) = Leaf x
treeConcat Empty (Node xs x ys) = Node xs x ys
treeConcat (Node xs x ys) Empty = Node xs x ys
treeConcat (Leaf x) Empty = Leaf x
treeConcat (Leaf x) (Leaf y) = Leaf (x ++ y)
treeConcat (Leaf y) (Node xs x ys) = Node  xs (y ++ x) ys
treeConcat (Node xs x ys) (Leaf y) = Node xs (x ++ y)  ys
treeConcat (Node xs x ys) (Node xs' x' ys') = Node (treeConcat xs xs') (x ++ x') (treeConcat ys ys')


listToTree :: [ Tree a] -> Tree [a]
--listToTree [] = Empty
--listToTree (x:xs) = treeConcat (toListTree x) (listToTree xs)

listToTree = foldr (treeConcat.toListTree) Empty

-- what traverse.traverse do?
-- allow us traversing a nested traversable set

-- Implement traverse in terms of two traversable structure

traverse' :: (Applicative f, Traversable t) => (a -> f b) -> t a -> f (t b)
traverse' f = sequenceA .fmap f

sequenceA' :: (Applicative f, Traversable t) => t (f a) -> f (t a)
sequenceA' = traverse' id

-- Implement fmap and foldMap using only traversable methods

-- fmap' :: Applicative f => Traversable f =>(a -> b) -> f a -> f b
-- fmap' g xs  =  traverse' (pure  g) xs 
-- 
-- Implement traversable instances of [], Maybe, ((,) e), and Either e
traverseList:: (Applicative f) => (a -> f b) -> [a] -> f [b]
-- traverseList _ [] = pure []
--traverseList g (x:xs) = ((g x) :  (traverseList g xs) )
traverseList f = foldr (\x xs -> (:) <$> (f(x)) <*> xs) (pure [])

traverseMaybe :: (Applicative f) => (a -> f b) -> Maybe a -> f (Maybe b)
traverseMaybe _ Nothing = pure Nothing
traverseMaybe g (Just x) = Just <$> (g x)

traverseTuple :: (Applicative f) => (a -> f b) -> (e, a) -> f (e, b)
traverseTuple g (e, x) = ((,) e) <$> g x

traverseEither :: (Applicative f) => (a -> f b) -> Either e a -> f (Either e b)
traverseEither _ (Left e) = pure (Left e)
traverseEither f (Right x) = Right <$> f x
-- Explain why set is foldable but not traversable
-- To be a traversable container, it must be Foldable and Functor. But set can not be a functor.
-- The problem with sets they do not comply with the composition law (fmap (g.h) = (fmap g). (fmap h))

-- Show that traversable functors compse: that is implement an instance for traversable (Compose f g)
-- Compose
newtype Compose f g a = Compose { getCompose :: f (g a) } deriving (Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
  fmap f (Compose x) = Compose ((fmap . fmap) f x)

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
  foldMap f (Compose x) = (foldMap . foldMap) f x

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
  traverse f (Compose t) = Compose <$> (traverse . traverse) f t