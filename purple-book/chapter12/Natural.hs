data Nat = 
 Zero
 | Succ Nat
 deriving (Eq, Show)

maybeExtract :: Maybe dat -> dat
maybeExtract Nothing = error "No data pesent"
maybeExtract (Just dat) = dat

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger(x)

integerToNat :: Integer -> Maybe Nat
integerToNat int 
 | int < 0 = Nothing
 | int == 0 = Just Zero
 | int > 0 = Just (Succ ( maybeExtract (integerToNat (int-1))))
