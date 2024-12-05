-- A simple test of the "exact printer".

import Curry.Comment         ( readComments )
import Curry.ExactPrint
import Curry.ExactPrintClass ( exactPrint )
import Curry.Files           ( readFullAST )

import Control.Monad         ( (<=<) )
import Test.Prop             ( returns, PropIO )

check :: String -> PropIO
check x = runTest x `returns` True

testExactPrintingMPTC :: PropIO
testExactPrintingMPTC = check "MPTC.curry"

testExactPrintingList :: PropIO
testExactPrintingList = check "List.curry"

testExactPrintingGuards :: PropIO
testExactPrintingGuards = check "Guards.curry"

testExactPrintingIfThenElse :: PropIO
testExactPrintingIfThenElse = check "IfThenElse.curry"

testExactPrintingCase :: PropIO
testExactPrintingCase = check "Case.curry"

testExactPrintingNoComments :: PropIO
testExactPrintingNoComments = check "NoComments.curry"

testExactPrintingInfix :: PropIO
testExactPrintingInfix = check "Infix.curry"

testExactPrintingListComp :: PropIO
testExactPrintingListComp = check "ListComp.curry"

testExactPrintingRecord :: PropIO
testExactPrintingRecord = check "Record.curry"

testExactPrintingDo :: PropIO
testExactPrintingDo = check "Do.curry"

testExactPrintingTupleTypes :: PropIO
testExactPrintingTupleTypes = check "TupleTypes.curry"

testExactPrintingFunction :: PropIO
testExactPrintingFunction = check "Function.curry"

testExactPrintingAsPattern :: PropIO
testExactPrintingAsPattern = check "AsPattern.curry"

testExactPrintingImports :: PropIO
testExactPrintingImports = check "Imports.curry"

testExactPrintingExports :: PropIO
testExactPrintingExports = check "Exports.curry"

testExactPrintingNonDet :: PropIO
testExactPrintingNonDet = check "NonDet.curry"

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
