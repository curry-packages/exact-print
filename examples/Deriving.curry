{-
  This module demonstrates the use of `deriving` in Curry.
-}

module Deriving where

data MyType1 = MyType Int String
  deriving Eq

data MyType2 = MyType2 Int String
  deriving (Eq, Show)

data MyType3 = MyType3 Int String
  deriving (Eq, Show, Ord)

data MyType4 = MyCons1 | MyCons2 | MyCons3 deriving (Eq, Show)

data MyType5 = MyType5
  deriving (Eq)

data MyType6 = MyType6 
  deriving (  Eq  , Show  )

data MyType7 = MyType7 
  deriving ( Eq
           , Show
           , Read )