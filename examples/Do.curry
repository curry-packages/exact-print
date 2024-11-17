{-
  Some examples of do notation (`IO` and `Maybe` monad).
-}
module Do where

getName :: IO String
getName = do
  putStrLn   "Wat's your name?"
  name <- getLine
  putStrLn $ "Hey, " ++ name ++ "!"
  return name

expr1 :: Maybe Int
expr1 = do
  x <- Just 3
  y <- Just 4
  return (x + y)