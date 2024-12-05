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