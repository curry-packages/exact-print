
-- Concatenating two lists:
-- (predefined as `++' in the standard prelude)

append :: [t] -> [t] -> [t]
append []     x  = x
append (x:xs) ys = x : append xs ys
