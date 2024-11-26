{-# OPTIONS_GHC -Wall #-}

module Ps6 where

data InfList a = Cons a (InfList a)

-- This function converts an InfList to a nromal Haskell lsit
infToList :: InfList a -> [a]
infToList (Cons x xs) = x : infToList xs

-- This function generates an infinite list of a given entry 
infRepeat :: a -> InfList a
infRepeat x = Cons x (infRepeat x)

-- Take first 10 elements 
instance (Show a) => Show (InfList a) where 
    -- show :: Show a => InfList a -> String
    show xs = show (take 20 (infToList xs))

-- This function applies an unpacked function to InfList 
infMap :: (a -> b) -> InfList a -> InfList b
infMap f (Cons x xs) = Cons (f x) (infMap f xs)

-- This function generates an infinite list from a seed
infFromSeed :: (a -> a) -> a -> InfList a
infFromSeed f seed = Cons seed (infFromSeed f (f seed))

-- This function generates list of natural numbers
nats :: InfList Integer
nats = infFromSeed (+1) 1

-- This function generates the infinite list ruler such that the n-th element 
-- is the highest power of 2 that divides n
ruler :: InfList Integer
ruler = interleave (infRepeat 0) (infMap (+1) ruler)
    where 
        interleave :: InfList a -> InfList a -> InfList a
        interleave (Cons x xs) ys = Cons x (interleave ys xs)
