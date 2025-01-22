{-# LANGUAGE FunctionalPatterns #-}
{-
  Examples of non-deterministic computations. Particularly, the exact-printing
  of free variables and functional patterns is tested.
-}

module NonDet where

nondet1 :: Int
nondet1 = 1 ? 2 ? 3

nondet2 :: Maybe Bool
nondet2 = x
 where x free 
 
nondet3 :: Either Bool ()
nondet3 = let x free in x 

anyPrefix :: Data a => [a] -> [a]
anyPrefix (x ++ _) = x