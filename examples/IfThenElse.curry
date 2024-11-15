{-
  Different ways of using if-then-else expressions.
-}
module IfThenElse where

myFun1 :: Int -> Int
myFun1 x = if x > 0 then x else -x

myFun2 :: Int -> Int
myFun2 x 
 = if x > 0 
    then x 
    else -x