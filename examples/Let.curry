module Let where

myFun1 :: Int -> Int -> Int
myFun1 x y = let z = x + y in
  if z > 0 then z else -z

myFun2 :: Maybe Int -> Int -> Int
myFun2 mx y = let Just a = mx
                  b = y + a 
              in if b > 0 then b else -b

myConst :: Int
myConst = let { x = 4815; y = 162342 } in x * 1000000 + y