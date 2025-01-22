{-
  Some examples of list comprehensions.
-}

module ListComp where

myExp1 :: [Int]
myExp1 = [2^x | x <- [1..10], even x]

myExp2 :: [[Int]]
myExp2 = [ x | x <- [[1..3], [4..9], [42..73]]
             , y <- x
             , length x + y > 5 ]

myExp3 :: [[Int]]
myExp3 = [            x | x <- [[1..3], [4..9], [42..73]]
           , y <- x ,
                                                          length x + y > 5 ]

mySingletonOrEmpty :: Bool -> a -> [a]
mySingletonOrEmpty b x = [x | b]