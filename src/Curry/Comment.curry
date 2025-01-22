module Curry.Comment where

import System.Directory    ( doesFileExist, getFileWithSuffix )
import System.FilePath     ( takeFileName, (</>), (<.>) )
import System.CurryPath    ( lookupModuleSourceInLoadPath, getLoadPathForModule
                           , inCurrySubdir, stripCurrySuffix )
import System.FrontendExec ( FrontendParams, FrontendTarget (..), defaultParams
                           , setQuiet, callFrontend, callFrontendWithParams )
import ReadShowTerm        ( readUnqualifiedTerm )

import Curry.Span 

data Comment = NestedComment String
             | LineComment String
  deriving (Eq, Ord, Show, Read)

commentString :: Comment -> String
commentString (NestedComment s) = s
commentString (LineComment   s) = s

-- | Reads the comments from a specified module
readComments :: String -> IO [(Span, Comment)]
readComments progname =
   readCommentsWithParseOptions progname (setQuiet True defaultParams)

-- | Reads the comments with further options from a specified module
readCommentsWithParseOptions :: String -> FrontendParams -> IO [(Span, Comment)]
readCommentsWithParseOptions progname options = do
  mbsrc <- lookupModuleSourceInLoadPath progname
  case mbsrc of
    Nothing -> do -- no source file, try to find Comments file in load path:
      loadpath <- getLoadPathForModule progname
      filename <- getFileWithSuffix (commentsFileName (takeFileName progname)) [""]
                                    loadpath
      readCommentsFile filename
    Just (dir,_) -> do
      callFrontendWithParams COMMS options progname
      readCommentsFile (commentsFileName (dir </> takeFileName progname))

-- | Get the comments filename of a curry programm
commentsFileName :: String -> String
commentsFileName prog = inCurrySubdir (stripCurrySuffix prog) <.> "cycom"

-- | Reads the comments from a specified file
readCommentsFile :: String -> IO [(Span, Comment)]
readCommentsFile filename = do
  filecontents <- readCommentsFileRaw filename
  if noComments filecontents
    then return []
    else return (readUnqualifiedTerm ["Curry.Span", "Curry.Position", "Curry.Comment"]
                                     filecontents)
 where 
  -- Checks if the `.cycom` file contains no comments (empty list). 
  -- This is true if the file contains nothing but whitespaces, 
  -- newlines, and brackets (string-representation of expression :: `[(Span, Comment)]`).
  --
  -- This check is necessary, unfortunately, because the parser fails for empty lists other 
  -- than "[]".
  noComments = all (`elem` "[ ]\n")

-- | Reads the text from a specified file containing comments
readCommentsFileRaw :: String -> IO String
readCommentsFileRaw filename = do
  extfcy <- doesFileExist filename
  if extfcy
   then readFile filename
   else do let subdirfilename = inCurrySubdir filename
           exdirtfcy <- doesFileExist subdirfilename
           if exdirtfcy
            then readFile subdirfilename
            else error ("EXISTENCE ERROR: Comment file '" ++ filename ++
                        "' does not exist")
