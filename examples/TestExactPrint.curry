-- A simple test of the "exact printer".

import Curry.Comment         ( readComments )
import Curry.ExactPrint
import Curry.ExactPrintClass ( exactPrint )
import Curry.Files           ( readFullAST )

--- Reads the AST and comments of a module and prints them
--- as the original source program.
printAST :: String -> IO ()
printAST str =  do
  ast   <- readFullAST str  
  comms <- readComments str  
  putStrLn $ exactPrint ast comms 

-- ...the same with debug output
printASTDebug :: String -> IO ()
printASTDebug str =  do
  ast   <- readFullAST str
  print ast
  comms <- readComments str
  print comms
  putStrLn $ exactPrint ast comms
