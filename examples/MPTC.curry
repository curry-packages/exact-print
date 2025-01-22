{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies, FlexibleInstances #-}
{-
  Different type class and instance declarations such as multi-parameter type classes
  with functional dependencies.
-}

module MPTC where

data MyData  a = MyCons a Int
data MyDataA _  
data MyDataB _  
data MyDataC _ 
data MyDataD
data MyDataE

class MyCoerce a b where
  myFun :: a -> b 

-- Superfluous brackets for instance type:
instance Eq a => Eq ((MyDataA a)) where 
  _ == _ = True

-- Context has single constraint and is not bracketed:
instance Eq a => Eq (MyDataB a) where 
  _ == _ = True

-- Context has single constraint and is bracketed:
instance (Eq a) => Eq (MyDataC a) where 
  _ == _ = True

-- Multiple type constraints and instance types: 
instance (Eq a, Eq b) => MyCoerce (MyData a) (MyData b) where
  myFun (MyCons _ _) = MyCons failed failed

-- No type constraints:
instance Eq MyDataD where
  _ == _ = True

instance Eq a => Eq (MyData a) where
  (MyCons x1 x2) == (MyCons y1 y2) = x1 == y1 && x2 == y2

-- Empty list of type constraints:
instance () => Eq MyDataE where
  _ == _ = True

------------------------------------
-- Various functional dependencies 

-- Functional dependency:
class MyCoerceUnique a b | a -> b where
  myFunUnique :: a -> b

-- Empty functional dependency:
class MyClassA a b c | -> where 
  myCFunA :: a -> b -> c

-- Functional dependency with only one type variable on the left:
class MyClassB a b c | a -> where 
  myCFunB :: a -> b -> c

-- Functional dependency with only one type variable on the right:
class MyClassC a b c | -> c where 
  myCFunC :: a -> b -> c

-- Functional dependency with multiple type variables on the left:
class MyClassD a b c | a b -> c where 
  myCFunD :: a -> b -> c

-- Multiple functional dependencies:
class MyClassE a b c | a -> c, c -> b where 
  myCFunE :: a -> b -> c

instance MyCoerceUnique (MyData Int) (MyData Float) where
  myFunUnique (MyCons x y) = MyCons (toFloat x) y