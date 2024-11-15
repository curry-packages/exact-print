module Guards where

import Data.Maybe ( fromJust, isJust )

myFun1 :: Int -> Int -> Int
myFun1 x y | x > 0     = x + y
           | otherwise = x - y

myFun2 :: Maybe Int -> Int -> Int
myFun2 mx y 
  | isJust mx = 42
  | otherwise = y

myFun3 :: [Int] -> Int
myFun3 xs          | null xs   = 0
                   | otherwise = head xs
