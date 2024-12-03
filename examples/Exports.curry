module Exports ( module Data.List
               , Function, Argument, MyExpr(..)
               , mySum, myMax 
               , myEval ) where

import Data.List ( (\\) )

type Function = [Int] -> Int
type Argument = MyExpr

mySum :: Function
mySum = foldr (+) 0

myMax :: Function
myMax = foldr max 0

data MyExpr = Num          Int 
            | Add          MyExpr MyExpr
            | FunctionCall Function [Argument]

myEval :: MyExpr -> Int
myEval (Num n)               = n
myEval (Add e1 e2)           = myEval e1 + myEval e2
myEval (FunctionCall f args) = f (map myEval args)

test :: Int
test = myEval (FunctionCall myMax [FunctionCall mySum [Num 1, Num 3], Num 3])