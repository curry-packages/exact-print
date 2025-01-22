{-
  Some examples for case expressions. 
-}

module Case where

myFromJust :: Maybe a -> a
myFromJust m = case m of
  Just y  -> y
  Nothing -> error "myFromJust: Nothing"

myFun1 :: Int -> Int -> Int
myFun1 = \x y -> case ((x + y)) of
  z | z > 0     -> z
    | otherwise -> -z

myFun2 :: [Int] -> Int
myFun2 xs 
 = case xs of
    []    -> 0
    (x:_) -> x

myFun3 :: Int -> Int
myFun3 x = case x of
             0 -> 0
             1 -> 1
             2 -> 2
             _ -> -1

myFun4 :: Int -> Int
myFun4 x = case () of _ 
                        | x == 0 -> 0
                        | x == 1 -> 1
                        | x == 2 -> 2
                        | otherwise -> -1

myFun5 :: Int -> Int
myFun5 x = case x of { 0 -> 0; 1 -> 1; 2 -> 2; _ -> -1 }