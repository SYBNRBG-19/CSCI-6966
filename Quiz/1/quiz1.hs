type DList a = [a] -> [a]

toList :: DList a -> [a]
toList x = x []

empty :: DList a
empty = \x -> []

singleton :: a -> DList a
singleton x = \_ -> [x]

append :: DList a -> DList a -> DList a
append xs ys = \_ -> xs [] ++ ys []

cons :: a -> DList a -> DList a
cons x xs = \_ -> x : xs []

fromList :: [a] -> DList a
fromList xs = foldr cons empty xs