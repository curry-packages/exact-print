{-
  Some examples of tuple types, patterns and expressions.
-}
module TupleTypes where

test1 :: IO ()
test1 = putStrLn "Hello, World!"

test2 :: IO (  )
test2 = putStrLn "Hello, World!"

fun1 :: (Int, Int) -> Int
fun1 (x, y) = x + y

fun2 :: ( Int, Int ) -> Int
fun2 ( x, y ) = x + y

-- `()` as a type expression and pattern:
fun3 :: () -> Int
fun3 () = 42

fun4 :: ( ) -> Int
fun4 ( ) = 73

-- `()` as an expression:
fun5 :: Int -> Int
fun5 x = case () of _ 
                      | x == 0    ->  0 
                      | x == 1    ->  1 
                      | otherwise -> -1

fun6 :: ( (Int, Int) ) -> Int
fun6 (x, _) = x