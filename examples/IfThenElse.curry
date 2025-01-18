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

myFun3 :: Int -> Int
myFun3 x = ifThenElse (x > 0) x (-x)

myFun4 :: Int -> Int
myFun4 x = if      x > 0  then 42 
           else if x < 0  then 73
           else if x == 0 then 4815162342
           else error "myFun4: unexpected value"