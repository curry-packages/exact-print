module Curry.ExactPrintClass
 ( ExactPrint(keywords, printS, printN), PrintAt(..), PutExact, Exact
 , exactPrint, printNode, empty, fill, noChilds, printStringAt, printListAt
 ) where

import List

import Curry.Types
import Curry.Comment
import Curry.Position
import Curry.Span
import Curry.SpanInfo

-- ShowS for efficiency.
type EPS = [(Span, String)] -> Position -> (ShowS, [(Span, String)], Position)

-- Monadic EPS
newtype EPSM a = EPSM (a, EPS)

type PutExact = EPSM ()

newtype Exact = Exact {
    unExact :: PutExact
  }

instance Functor EPSM where
  fmap f (EPSM (a, x)) = EPSM (f a, x)

instance Monad EPSM where
  return a = EPSM (a, emptyEPS)
  EPSM (a, eps1) >>= f = let EPSM (b, eps2) = f a in EPSM (b, eps1 <+> eps2)
  EPSM (_, eps1) >> EPSM (b, eps2) = EPSM (b, eps1 <+> eps2)

exactPrint :: ExactPrint a => a -> [(Span, Comment)] -> String
exactPrint = exactPrintFrom 1

exactPrintFrom :: ExactPrint a => Int -> a -> [(Span, Comment)] -> String
exactPrintFrom l a cs =
  let EPSM (_, f) = printNode a in fst3 (f cs' (Position l 1)) ""
  where cs' = map (\(sp,c) -> (sp, commentString c)) cs -- comments are just kws
        fst3 (x,_,_) = x

class PrintAt a where
  printString :: a -> String
  printSpan   :: a -> Span

class HasSpanInfo a => ExactPrint a where
  keywords :: a -> [String]
  printS   :: a -> Exact

  -- Adds keywords to the whitespace-replacement of a EPS computation
  -- and fills Whitespace up to the beginning
  printN :: a -> PutExact
  printN a = liftEPS $ withSrcInfoPoints (getSpanInfo a) (keywords a) eps
    where
      EPSM (_, eps) = do
        liftEPS $ fillUpS (getStartPosition a) -- fill space before entity
        unExact $ printS  a                    -- collect keywords to print
        liftEPS $ fillUpS (getEndPosition   a) -- print them, add trailing space

instance ExactPrint a => ExactPrint [a] where
  printS      = Exact . sequenceExact
  keywords _  = []
  printN   xs = sequenceExact xs

printStringAt :: Span -> String -> PutExact
printStringAt sp s =
  liftEPS $ withSrcInfoPoints (SpanInfo sp [sp]) [s] eps
  where
    EPSM (_, eps) = do
      liftEPS $ fillUpS (start sp) -- fill space before string
      empty                        -- no other keywords to print
      liftEPS $ fillUpS (end   sp) -- print string, add trailing space

printListAt :: PrintAt a => [a] -> PutExact
printListAt []     = empty
printListAt (x:xs) =
  printStringAt (printSpan x) (printString x) >> printListAt xs

printNode :: ExactPrint a => a -> PutExact
printNode = printN

empty :: PutExact
empty = liftEPS emptyEPS

fill :: PutExact -> Exact
fill = Exact

noChilds :: Exact
noChilds = Exact empty

-------------------------------------------------------------------------
-- Internals
-------------------------------------------------------------------------

liftEPS :: EPS -> PutExact
liftEPS e = EPSM ((), e)

--------------------
-- Combinators
--------------------

(<+>) :: EPS -> EPS -> EPS
eps1 <+> eps2 = \cs1 p1 -> let (s2,      cs2, p2) = eps1 cs1 p1
                               (s3,      cs3, p3) = eps2 cs2 p2
                           in  (s2 . s3, cs3, p3)

replicateS :: Int -> Char -> ShowS
replicateS n c | n <  0    = error "negative value"
               | n == 0    = id
               | otherwise = showChar c . replicateS (n-1) c

emptyEPS :: EPS
emptyEPS cs p = (id, cs, p)

sequenceExact :: ExactPrint a => [a] -> PutExact
sequenceExact = sequence_ . map printNode

--------------------------------------------
-- implementation details for exactPrintFull
--------------------------------------------

-- open a new scope for the new computation and add the new keywords
-- to the list of (filtered) keywords just for this scope.
-- Use the rest when exiting this scope
withSrcInfoPoints :: SpanInfo -> [String] -> EPS -> EPS
withSrcInfoPoints NoSpanInfo        _        _   _  _ = error "NoSpanInfo"
withSrcInfoPoints (SpanInfo sp sps) keyws eps cs p =
  replaceComments $ eps (merge (zip sps keyws) bef) p
  where (aft, bef) = partition ((`isAfter` sp) . fst) cs
        replaceComments (s, _, p') = (s, aft, p')

merge :: [(Span, a)] -> [(Span, a)] -> [(Span, a)]
merge []          []       = []
merge xs@(_:_)    []       = xs
merge []          ys@(_:_) = ys
merge ((sx,x):xs) ((sy,y):ys)
        | sx `isBefore` sy = (sx, x) : merge xs ((sy,y):ys)
        | otherwise        = (sy, y) : merge ((sx,x):xs) ys

fillUpS :: Position -> EPS
fillUpS fillEnd []          p = case (p, fillEnd) of
    (Position l1 c1, Position l2 c2)
      | l1 <  l2  -> whitespaceUntil (l2 - l1) (c2 - 1) [] fillEnd
      | l1 == l2 &&
        c1 <= c2  -> whitespaceUntil 0 (c2 - c1) [] fillEnd
      | otherwise -> (id, [], p)
    _             -> error "No Positional information available"
fillUpS fillEnd ((sp,c):cs) p = case (sp, fillEnd) of
    (Span (Position l1 c1) _, Position l2 c2)
      | l1 <  l2 ||
       (l1 == l2 &&
        c1 <= c2) -> (exactPrintOther (sp, c) <+> fillUpS fillEnd) cs p
      | otherwise -> replaceComments $ fillUpS fillEnd [] p
    _             -> error "No Positional information available"
  where replaceComments (s, _, p') = (s, (sp,c):cs, p')

exactPrintOther :: (Span, String) -> EPS
exactPrintOther (sp, s) cs p =
  let (s', _, _) = fillUpS (start sp) [] p
  in (s' . showString s, cs, incr (end sp) 1)

whitespaceUntil :: Int -> Int -> EPS
whitespaceUntil l c cs p = (replicateS l '\n' . replicateS c ' ', cs, p)
