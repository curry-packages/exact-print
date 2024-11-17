-- A simple test of the "exact printer".

import Curry.Comment         ( readComments )
import Curry.ExactPrint
import Curry.ExactPrintClass ( exactPrint )
import Curry.Files           ( readFullAST )

import Control.Monad         ( (<=<) )
import Test.Prop             ( returns, PropIO )

testExactPrintingMPTC :: PropIO
testExactPrintingMPTC = runTest "MPTC.curry" `returns` True

testExactPrintingList :: PropIO
testExactPrintingList = runTest "List.curry" `returns` True

testExactPrintingGuards :: PropIO
testExactPrintingGuards = runTest "Guards.curry" `returns` True

testExactPrintingIfThenElse :: PropIO
testExactPrintingIfThenElse = runTest "IfThenElse.curry" `returns` True

testExactPrintingCase :: PropIO
testExactPrintingCase = runTest "Case.curry" `returns` True

testExactPrintingNoComments :: PropIO
testExactPrintingNoComments = runTest "NoComments.curry" `returns` True

testExactPrintingInfix :: PropIO
testExactPrintingInfix = runTest "Infix.curry" `returns` True

testExactPrintingListComp :: PropIO
testExactPrintingListComp = runTest "ListComp.curry" `returns` True

testExactPrintingRecord :: PropIO
testExactPrintingRecord = runTest "Record.curry" `returns` True

-- Compares the content of a (hand-written) curry source file to the exact-printed module.
runTest :: String -> IO Bool
runTest str = do
  ep <- cleanup <$> getAST str
  lf <- cleanup <$> readFile str

  return (ep == lf)
 where 
  -- Removes trailing whitespaces and trailing empty lines from the curry source. 
  -- This is necessary because the exact-printer does not* add trailing whitespaces 
  -- or empty trailing lines to the output, and string equality would fail otherwise.
  --
  -- * Because empty trailing whitespaces of comments are added to the output, 
  --   this should be applied to the exact-printed module aswell. Thus, this test 
  --   omits checking if comments have "enough" trailing whitespaces, which does not 
  --   change the validity of the test
  cleanup            = unlines . dropWhitespaces . dropEmptyLinesBack . lines
  dropWhitespaces    = map (reverse . dropWhile (==' ') . reverse)
  dropEmptyLinesBack = reverse . dropWhile (=="") . reverse

--- Reads the AST and comments of a module returns the exact-printed module.
getAST :: String -> IO String
getAST str =  do
  ast   <- readFullAST str  
  comms <- readComments str  
  return $ exactPrint ast comms 

-- Shows the exact-printed module.
printAST :: String -> IO ()
printAST = putStrLn <=< getAST

-- ...the same with debug output
printASTDebug :: String -> IO ()
printASTDebug str =  do
  ast   <- readFullAST str
  print ast
  comms <- readComments str
  print comms
  putStrLn $ exactPrint ast comms
