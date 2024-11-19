{-
  Examples of function definitions and applications. 
-}

module Function where

($+):: Int -> Int -> Int
($+) = (+)

(%+):: Int -> Int -> Int
x %+ y = x + y

myConst :: Int
myConst = 42

myId :: a -> a
myId x = x

myAdd :: Int -> 
         Int ->
         Int
myAdd = (+) 

myAddFull :: Int -> Int -> Int
myAddFull x y = x + y

myIncrL :: Int -> Int
myIncrL = (1 +)

myIncrR :: Int -> Int
myIncrR = (+ 1)

myVar :: Int
myVar = let val = 73 in (val)

myFun :: Int -> Int
myFun = localFun
  where
    localFun :: Int -> Int
    localFun _ = myConst

myVals :: [Int] -> [Int]
myVals = filter even . map myFun

myExp :: (Enum a, Num a) => [Maybe a]
myExp = map Just $ [1 .. 10] ++ [11..20] ++ [  21  ..  30  ]