{-# OPTIONS_GHC -Wall #-}

module Ps4MergeSort where

import Prelude hiding ( foldr, length, filter, minimum )
import qualified Data.List as List

newtype MergesortList a = ML [a] deriving (Eq,Show)

singleton :: a -> MergesortList a
singleton a = ML [a]

toList :: MergesortList a -> [a]
toList (ML a) = a

fromList :: Ord a => [a] -> MergesortList a
fromList = foldList . map singleton

foldr :: (a -> b -> b) -> b -> MergesortList a -> b
foldr f b (ML xs) = List.foldr f b xs

filter :: (a -> Bool) -> MergesortList a -> MergesortList a
filter p (ML xs) = ML (List.filter p xs)

length :: MergesortList xs -> Int
length (ML xs) = List.length xs

instance Ord a => Semigroup (MergesortList a) where
    -- Implementation of the associative appending operation. 
    -- (<>) :: Ord a => MergesortList a -> MergesortList a -> MergesortList a
    ml1 <> ml2 = ML (merge (toList ml1) (toList ml2))
        where
            -- This function merges two lists based into an ordered one. 
            merge :: Ord a => [a] -> [a] -> [a]
            merge [] bs = bs
            merge as [] = as
            merge (a:as) (b:bs)
                | a <= b = a : merge as (b:bs)
                | otherwise = b : merge (a:as) bs

instance Ord a => Monoid (MergesortList a) where
    -- Identity Element under (<>)
    -- mempty :: Ord a => MergesortList a
    mempty = ML []

foldList :: Monoid b => [b] -> b
foldList = List.foldr (<>) mempty

-- This function selects the minimum element in a MergesortList. 
-- If the list is empty, it returns mempty. 
minimum :: MergesortList a -> Maybe a
minimum m = case m of
    ML [] -> Nothing
    ML (x:_) -> Just x

-- This function counts the number of distinct elements in a MergesortList. 
numDistinct :: Ord a => MergesortList a -> Int
numDistinct m = case m of
    ML [] -> 0
    ML [x] -> 1
    ML (x1:x2:xs) -> if x1 == x2 then numDistinct ms else 1 + numDistinct ms
        where ms = ML (x2:xs)

-- This function counts the occurances of each entry in the list. 
count :: Eq a => MergesortList a -> MergesortList (a,Integer)
count ml = case ml of
    ML [] -> ML []
    ML (m:ms) -> ML (counting ms m 1)
        where
            -- This function recursively does the job. 
            counting :: Eq a => [a] -> a -> Integer -> [(a, Integer)]
            counting [] cur c = [(cur, c)]
            counting (x:xs) cur c
                | x == cur = counting xs cur (c + 1)
                | otherwise = (cur, c) : counting xs x 1

----- Sorting functionality -----

newtype DivideList a = DL { getDivideList :: [a] } deriving (Eq,Show)

-- This function splits the DivideList from the middle entry. 
divide :: DivideList a -> (DivideList a, DivideList a)
divide dl = (DL ldl, DL rdl)
    where
        l = getDivideList dl
        (ldl, rdl) = splitAt (List.length l `div` 2) l

instance Foldable DivideList where
    -- foldMap :: Monoid m => (a -> m) -> DivideList a -> m
    foldMap f xs = case xs of
        DL [] -> mempty
        DL [x] -> f x
        _ -> foldMap f ls <> foldMap f rs where (ls, rs) = divide xs

-- This functions invokes previous foldMap to sort the list.
mergeSort :: Ord a => [a] -> [a]
mergeSort xs = toList $ foldMap singleton (DL xs)
