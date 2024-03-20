{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
class Semigroup a where
    (<>) :: a -> a -> a
class Monoid a where
    mempty :: a 
    mappend :: a -> a -> a

-- implement monoid and semigroup instances of the following:
data Identity a = Identity a -- cant define monoid or semigroup of it because this is a single element, not a group of elements
-- is this correct understanding?
data Maybe a = Just a | Nothing
data List a = Cons a (List a) | Nil
data Either a b = Right a | Left b

instance Monoid a => Monoid (Maybe a) where  -- had to assume that a is also an monoid.
    mempty = Nothing
    mappend Nothing _ = Nothing
    mappend _ Nothing = Nothing
    mappend (Just x) (Just y) = Just (mappend x y)

instance Monoid (List a) where
    mempty = Nil
    mappend Nil Nil = Nil
    mappend Nil xs = xs
    mappend xs Nil = xs
    mappend (Cons x (xs)) (Cons y (ys)) = (Cons x (mappend xs (Cons y (ys))))

instance Monoid e => Monoid (Either e a) where
  mempty = Right mempty  -- Use the mempty value from the underlying monoid
  mappend (Left l) _ = Left l
  mappend _ (Left l) = Left l
  mappend (Right x) (Right y) = Right (mappend x y) -- here also using the mappend from the Monoid a


instance Semigroup a => Semigroup (Maybe a) where
   Nothing <> _ = Nothing
   _ <> Nothing = Nothing
   (Just x) <> (Just y) = Just (x <> y)

instance Semigroup (List a) where
    Nil <> x = x
    x <> Nil = x
    (Cons x (xs)) <> (Cons y (ys) ) = (Cons x (xs <> (Cons y (ys))))

instance Semigroup e  => Semigroup (Either e a) where 
    (Left l) <> _ = (Left l)
    _ <> (Left l) = (Left l)
    (Right x) <> (Right y) = Right (x <> y)