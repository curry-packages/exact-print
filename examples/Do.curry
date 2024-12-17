{-
  Some examples of do notation (`IO` and `Maybe` monad).
-}
module Do where

getName :: IO String
getName = do
  putStrLn   "What's your name?"
  name <- getLine
  putStrLn $ "Hey, " ++ name ++ "!"
  return name

expr1 :: Maybe Int
expr1 = do
  x <- Just 3
  y <- Just 4
  return (x + y)

expr2 :: Maybe Int
expr2 = do { x <- Just 3; y <- Just 4; return (x + y) }

expr3 :: IO ()
expr3 = do { return () }

expr4 :: IO ()
expr4 = do {
  putStr   "Hello, ";
  putStrLn "World!";
  return ()
}

expr5 :: IO ()
expr5 = do 
      {
        putStr   "Hello, ";
        putStrLn "World!";
        return ()
      }