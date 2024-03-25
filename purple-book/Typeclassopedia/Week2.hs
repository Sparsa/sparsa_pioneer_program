import Control.Monad (liftM,ap)
import Control.Monad.Trans.Class

data IdentityT m a = IdentityT (m a)
data MaybeT m a = JustT (m a) | NothingT
data ListT m a = ConsT (m a) (ListT m a) | NilT

-- Week 1 instances : 
-- Semigroup, Monoid, Functor, Applicative, Monad, Foldable, Traversable
-- functr 

instance Functor m => Functor (IdentityT m) where
  fmap :: Functor m => (a -> b) -> IdentityT m a -> IdentityT m b
  fmap f (IdentityT ma) = IdentityT $ fmap (f) ma

instance Functor m => Functor (MaybeT m) where
    fmap :: Functor m => (a -> b) -> MaybeT m a -> MaybeT m b
    fmap _ NothingT = NothingT
    fmap f (JustT mx) = JustT $ fmap f mx

instance Functor m => Functor (ListT m) where
    fmap:: Functor m => (a -> b) -> ListT m a -> ListT m b
    fmap _ NilT = NilT
    fmap f (ConsT mx xs) = ConsT (fmap f mx)  $  fmap f xs

-- Applicative
instance Applicative m => Applicative (IdentityT m) where
    pure :: Applicative m => a -> IdentityT m a
    pure x = IdentityT (pure x )
    (<*>) :: Applicative m => IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
    (<*>) (IdentityT mf) (IdentityT ma) = IdentityT $ (<*>) mf (ma) 
instance Applicative m => Applicative (MaybeT m) where
    pure :: Applicative m => a -> (MaybeT m a) 
    pure x = JustT (pure x)
    (<*>) :: Applicative m => MaybeT m (a -> b) -> MaybeT m a -> MaybeT m b
    (<*>) NothingT _ = NothingT
    (<*>) _ NothingT = NothingT
    (<*>) (JustT mf) (JustT mx) = JustT $ (<*>) mf mx
instance Applicative m => Applicative (ListT m) where
    pure:: Applicative m => a -> ListT m a 
    pure x = ConsT (pure x) NilT
    (<*>) :: Applicative m => ListT m (a -> b) -> ListT m a -> ListT m b 
    (<*>) NilT _ = NilT 
    (<*>) _ NilT = NilT 
    (<*>) (ConsT f mf) (ConsT x mx) = ConsT ((<*>) f x) ((<*>) mf mx) 
-- Monoid 
-- No instance of identity
-- instance (Monoid m) => Monoid (MaybeT m a) where
--     mempty = NothingT 
--     mappend :: Monoid m => MaybeT m a -> MaybeT m a -> MaybeT m a
--     mappend NothingT _ = NothingT 
--     mappend _ NothingT = NothingT 
--     mappend (JustT xs) (JustT ys ) = JustT $ mappend xs ys 

-- Semigroup 
-- instance (Semigroup m) => Semigroup (MaybeT m a) where
--     (<>) :: (Semigroup m) => MaybeT m a -> MaybeT m a -> MaybeT m a 
--     (<>) NothingT _ = NothingT
--     (<>) _ NothingT = NothingT
--     (<>) (JustT xs) (JustT ys) = JustT $ (<>) xs ys 

-- Monad 
instance MonadTrans (MaybeT) where 
    lift :: Monad m => m a => (MaybeT m a)
    lift = JustT 
instance (Monad m ) => Monad (MaybeT m ) where 
    return :: (Monad m) => a -> MaybeT m a 
    return x = JustT (return x)
    (>>=) :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
    (>>=) NothingT _ = NothingT
    (>>=) (JustT mx) f = JustT $ (>>=) (lift f) mx
