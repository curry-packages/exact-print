{- 
  Some examples of imports and the usage of qualified identifiers.
-}
module Imports where 

import Prelude hiding ( readFile )
import Data.List      ( nub )
import Control.Monad  ( (<=<) ) 

import qualified Data.Maybe as M ( fromJust
                                 , isJust )

import qualified Control.Applicative as A hiding ( (<*>) )

myFromJust :: Maybe a -> a
myFromJust = M.fromJust

myElem :: Eq a => a -> [a] -> Bool
myElem x xs = x `Prelude.elem` xs 

loop :: _
loop = Imports.loop