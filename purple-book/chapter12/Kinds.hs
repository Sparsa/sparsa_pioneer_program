module Kinds where

import Prelude hiding (not)

not True = False
not False = True

data List a = Nil | Cons a (List a)

x :: Int
x = 5

y::List 
y = Nil

