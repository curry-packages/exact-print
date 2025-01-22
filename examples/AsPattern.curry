{-# OPTIONS_FRONTEND -Wno-incomplete-patterns -Wno-unused-bindings #-}

{-
  Some examples of as-patterns.
-}
module AsPattern where

myFun1 :: [Int] -> [Int]
myFun1 []         = []
myFun1 xs@(_:xs') = xs ++ myFun1 xs'

myFun2 :: Int -> Bool
myFun2 x@0 = True

myLocalAsPat :: Int -> Bool
myLocalAsPat x = let y@(Just val) = Just x in True

myCaseAsPat :: Either Int Int -> Bool
myCaseAsPat x = case x of
  y@(Left 0) -> True
  _          -> False