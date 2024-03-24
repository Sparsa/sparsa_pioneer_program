{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}
import GHC.Base 
import Test.QuickCheck
import Data.Eq ((==))

data Identity a = Id a
data List a = Cons a (List a) | Nil
data Maybe' a = Nothing' | Just' a
data Either a b = Left a | Right b

-- data Identity a = Id a


instance Arbitrary a => Arbitrary (Maybe' a) where
    arbitrary = oneof [ return Nothing', fmap Just' arbitrary]
instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
    arbitrary = oneof [ fmap Left arbitrary, fmap Right arbitrary]
-- I have implemented both genMaybe as I was getting confused by the arbitrary functions of different data types. 
-- Let me know which one is a better way to do it? In my understanding defining arbitrary is better because it will later help
-- to declare new class based on the exising one.
genMaybe :: (Arbitrary a) => Gen (Maybe a)
genMaybe = do
    x <- arbitrary
    oneof [return  (Just x), 
        return  Nothing]
instance Arbitrary a => Arbitrary (Identity a) where 
    arbitrary =  fmap Id arbitrary

genList :: (Arbitrary a) => Gen (List a) 
genList = do 
    x <- arbitrary
    oneof [return Nil, fmap (Cons x) genList] 

