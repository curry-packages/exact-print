{-
  Some examples of as-patterns.
-}
module AsPattern where

myFun1 :: [Int] -> [Int]
myFun1 []         = []
myFun1 xs@(_:xs') = xs ++ myFun1 xs'

myFun2 :: Int -> Bool
myFun2 x@0 = True