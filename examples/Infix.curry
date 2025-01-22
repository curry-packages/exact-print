{-
  Examples of infix operators in Curry. Also demonstrates the specification of
  precedence and associativity rules for infix operators (fixity).
-}
module Infix where

myExp :: Bool
myExp = '!' `elem` "Hello, World!"

-- Fixity declaration with multiple operators
infixl 5 ?., ??., ???. 
(?.), (??.), (???.) :: Int -> Bool
(?.)   = (== 0)
(??.)  = (== 1)
(???.) = (== 2)

-- InfixR with precedence information
infixr 4 ==>
(==>) :: Bool -> Bool -> Bool
a ==> b = not a || b

-- InfixL with precedence information
infixl 4 <==
(<==) :: Bool -> Bool -> Bool
a <== b = b ==> a

-- Infix without precedence information
infix =/=> 
(=/=>) :: Bool -> Bool -> Bool
a =/=> b = a ==> not b