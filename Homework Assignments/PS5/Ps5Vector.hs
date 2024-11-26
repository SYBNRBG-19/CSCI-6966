{-# LANGUAGE QuantifiedConstraints #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE BlockArguments #-}

module Ps5Vector where

import qualified Control.Monad as Monad (guard, ap)
import Data.Foldable (toList)
import qualified Data.Maybe as Maybe

class (Monad v, Foldable v, forall a. Monoid (v a)) => Vector v where
    first :: v a -> Maybe a
    final :: v a -> Maybe a
    index :: Int -> v a -> Maybe a
    insert :: Int -> a -> v a -> Maybe (v a)
    delete :: Int -> v a -> Maybe (v a)

-- Define the following list-like operations. As Vector is an instance
-- of Monad, Foldable and Monoid, give minimal definitions in terms of 
-- the Monad, Foldable, and Monoid functions. 

singleton :: Vector v => a -> v a
singleton x = return x

append :: Vector v => v a -> v a -> v a
append = (<>)

nil :: Vector v => v a
nil = mempty

cons :: Vector v => a -> v a -> v a
cons = (<>) . return

fromList :: Vector v => [a] -> v a
fromList = foldMap return

pairs :: Vector v => v a -> v b -> v (a,b)
pairs = Monad.ap . fmap (,)

concatMap :: Vector v => v a -> (a -> v b) -> v b
concatMap = (>>=)

-- Part (2)

-- Instantiate List as Vector. Use Monad.guard and >>= (or >>) to 
-- implement these operations.

instance Vector [] where
    -- first :: [a] -> Maybe a
    first ls = Monad.guard (not (null ls)) >> return (head ls)

    -- final :: [a] -> Maybe a
    final ls = Monad.guard (not (null ls)) >> return (last ls)

    -- index :: Int -> [a] -> Maybe a
    index i ls = g >> return (ls !! i)
        where g = Monad.guard (i >= 0 && i < length ls)

    -- insert :: Int -> a -> [a] -> Maybe [a]
    insert i x ls = g >> return (take i ls ++ [x] ++ drop i ls)
        where g = Monad.guard (i >= 0 && i <= length ls)

    -- delete :: Int -> [a] -> Maybe [a]
    delete i ls = g >> return (take i ls ++ drop (i + 1) ls)
        where g = Monad.guard (i >= 0 && i < length ls)

-- Part (3)

-- Now, instantiate a balanced-tree-based Vector. Implementing a 
-- list-based vector is straight-forward, however, operations "final", 
-- "index", "insert" and "delete" all take O(n) time in the worst case, 
-- where n is the length of the list. Using a balanced tree as an 
-- underlying representation trades the O(1)-time "first" operation for 
-- O(lg n)-time for _all_operations. 

-- The Vec data type for implementing a tree-based Vector

data Vec a = Empty
           | Tree (AVL a) -- non-empty tree structure with data at leaves
        deriving (Show)

-- AVL trees are _balanced_ trees that enable Vector ops in O(log n) time 
-- Here all data is stored at the leafs

data AVL a = Atom a -- leaf
           | Node
               Int -- cached number of elements
               Int -- cached height
               (AVL a) -- left branch
               (AVL a) -- right branch
        deriving (Show)

-- height is a O(1) operation
height :: AVL a -> Int
height (Atom _) = 1
height (Node _ k _ _) = k

-- An example instance of our Vector type
vec1 :: Vec Int
vec1 = Tree $ Node 3 3 (Node 2 2 (Atom 7) (Atom 3)) (Atom 4)

-- The AVL type should be foldable. Instantiate length and foldr.

instance Foldable AVL where
    -- length :: AVL a -> Int
    length (Atom _) = 1
    length (Node _ _ l r) = length l + length r

    -- foldr :: (a -> b -> b) -> b -> (AVL a) -> b 
    foldr f acc (Atom x) = f x acc
    foldr f acc (Node _ _ lt rt) = foldr f (foldr f acc rt) lt

-- Complete Vector the Cector instance. First, implement the Foldable, 
-- Monad and Monoid operations. Then implement the Vector functions.

instance Foldable Vec where
    -- length :: Vec a -> Int
    length Empty = 0
    length (Tree avl) = length avl

    -- foldr :: (a -> b -> b) -> b -> (Vec a) -> b
    foldr _ acc Empty = acc
    foldr f acc (Tree avl) = foldr f acc avl

-- Make Vec an instance of Eq. Use toList to implement equality as we 
-- don't care about the shape of the underlying structure. Note that 
-- toList is derived for both AVL and Vec as they are instances of 
-- Foldable.

instance Eq a => Eq (Vec a) where
    -- (==) :: Eq a => Vec a -> Vec a -> Bool
    (==) v1 v2 = toList v1 == toList v2

-- Make Vec an instance of Semigroup and Monoid.

-- (**) This is one case where you will need to rebalance the tree.

instance Semigroup (Vec a) where
    -- (<>) :: (Vec a) -> (Vec a) -> (Vec a)
    v1 <> v2 = case (v1, v2) of
        -- ignore empty trees
        (_, Empty) -> v1
        (Empty, _) -> v2
        -- merge two trees and keep them balanced
        (Tree t1, Tree t2) -> Tree (concatAVL t1 t2)

-- append two trees
concatAVL :: AVL a -> AVL a -> AVL a
concatAVL t1 t2
    | height t1 > height t2 + 1 = case t1 of
        Atom _ -> rebalance $ Node 0 0 t1 t2
        Node _ _ l r ->
            let newRight = concatAVL r t2
            in rebalance $ Node 0 0 l newRight
    | height t2 > height t1 + 1 = case t2 of
        Atom _ -> rebalance $ Node 0 0 t1 t2
        Node _ _ l r ->
            let newLeft = concatAVL t1 l
            in rebalance $ Node 0 0 newLeft r
    | otherwise = rebalance $ Node 0 0 t1 t2

instance Monoid (Vec a) where
    -- mempty :: Vec a
    mempty = Empty

-- Make Vec an instance of Monad. Since Monad extends Applicative, 
-- Functor and Applicative come first.

instance Functor Vec where
    -- fmap :: (a->b) -> Vec a -> Vec b
    fmap _ Empty = Empty
    fmap f (Tree avl) = Tree (fmapAVL f avl)
        where
            fmapAVL :: (a -> b) -> AVL a -> AVL b
            fmapAVL g (Atom x) = Atom (g x)
            fmapAVL g (Node n h l r) = Node n h (fmapAVL g l) (fmapAVL g r)

-- We'll cover Applicative functors later, so no need to worry about this now.

instance Applicative Vec where
    -- pure :: a -> Vec a
    pure = Tree . Atom

    -- (<*>) :: Vec (a -> b) -> Vec a -> Vec b
    (<*>) = Monad.ap

-- Use definition of >>= for ordinary lists as an ispiration (i.e., concatMap f xs) 
-- But, do not convert Vec trees into ordinary lists in your solution!

instance Monad Vec where
    -- return :: a -> Vec a
    return = pure

    -- (>>=) = Vec a -> (a -> Vec b) -> Vec b
    (>>=) Empty _ = Empty
    (>>=) (Tree t) f = foldr (\x acc -> f x <> acc) Empty t

instance Vector Vec where
    -- first :: Vec a -> Maybe a
    first = vecFirst

    -- final :: Vec a -> Maybe a
    final = vecFinal

    -- index :: Int -> Vec a -> Maybe a
    index = vecIndex

    -- insert :: Int -> a -> Vec a -> Maybe (Vec a)
    insert = vecInsert

    -- insert :: Int -> Vec a -> Maybe (Vec a)
    delete = vecDelete

-- access the first element, if there is one.
vecFirst :: Vec a -> Maybe a
vecFirst Empty = Nothing
vecFirst (Tree t) = takeFirst t
    where
        takeFirst :: AVL a -> Maybe a
        takeFirst (Atom x) = Just x
        takeFirst (Node _ _ l _) = takeFirst l

-- access the last element, if there is one.
vecFinal :: Vec a -> Maybe a
vecFinal Empty = Nothing
vecFinal (Tree t) = takeFinal t
    where
        takeFinal :: AVL a -> Maybe a
        takeFinal (Atom x) = Just x
        takeFinal (Node _ _ _ r) = takeFinal r

-- access the element specified by the index.
vecIndex :: Int -> Vec a -> Maybe a
vecIndex _ Empty = Nothing
vecIndex i (Tree t) = takeIndex i t
    where
        takeIndex :: Int -> AVL a -> Maybe a
        takeIndex 0 (Atom x) = Just x
        takeIndex _ (Atom _) = Nothing
        takeIndex idx (Node n _ l r)
            | idx < 0 || idx >= n = Nothing
            | idx < length l = takeIndex idx l
            | otherwise = takeIndex (idx - length l) r

-- ** Another operation where you will need to rebalance
vecInsert :: Int -> a -> Vec a -> Maybe (Vec a)
vecInsert i x Empty = Monad.guard (i == 0) >> return (Tree (Atom x))
vecInsert idx x' (Tree t) = do
    t' <- insertN idx x' t
    return (Tree t')
        where
            insertN :: Int -> a -> AVL a -> Maybe (AVL a)
            insertN i x y@(Atom _)
                | i == 0 = Just $ rebalance $ Node 2 2 (Atom x) y
                | i == 1 = Just $ rebalance $ Node 2 2 y (Atom x)
                | otherwise = Nothing
            insertN i x (Node _ _ l r)
                | i < 0 || i > length l + length r = Nothing
                | i <= length l = do
                    l' <- insertN i x l
                    return $ rebalance $ recalc $ Node 0 0 l' r
                | otherwise = do
                    r' <- insertN (i - length l) x r
                    return $ rebalance $ recalc $ Node 0 0 l r'

-- ** Another operation where you will need to rebalance
vecDelete :: Int -> Vec a -> Maybe (Vec a)
vecDelete _ Empty = Nothing
vecDelete idx (Tree t) = do
    mt' <- deleteN idx t
    case mt' of
        Nothing -> Just Empty
        Just t' -> Just (Tree t')

-- delete a node based on provided index.    
deleteN :: Int -> AVL a -> Maybe (Maybe (AVL a))
deleteN i (Atom _)
    | i == 0 = Just Nothing
    | otherwise = Nothing
deleteN i (Node _ _ l r)
    | i < 0 || i >= length l + length r = Nothing
    | i < length l = do
        ml' <- deleteN i l
        case ml' of
            Nothing -> Just (Just $ rebalance r)
            Just l' -> return $ Just $ rebalance $ recalc $ Node 0 0 l' r
    | otherwise = do
        mr' <- deleteN (i - length l) r
        case mr' of
            Nothing -> Just (Just $ rebalance l)
            Just r' -> return $ Just $ rebalance $ recalc $ Node 0 0 l r'

-- compute the balence factor
balanceFactor :: AVL a -> Int
balanceFactor (Atom _) = 0
balanceFactor (Node _ _ l r) = height l - height r

-- recalculate the size of AVL tree
recalc :: AVL a -> AVL a
recalc (Atom x) = Atom x
recalc (Node _ _ l r) = Node n h l r
  where
    n = length l + length r
    h = 1 + max (height l) (height r)

-- rebalance the updated tree
rebalance :: AVL a -> AVL a
rebalance node@(Atom _) = node
rebalance node@(Node _ _ l r)
    | balanceFactor node > 1 = if balanceFactor l >= 0
        then rotateRight node
        else rotateLeftRight node
    | balanceFactor node < -1 = if balanceFactor r <= 0
        then rotateLeft node
        else rotateRightLeft node
    | otherwise = if newFactor <= 1 && newFactor >= -1
        then newNode 
        else rebalance newNode
        where 
            l' = rebalance l
            r' = rebalance r
            newNode = recalc (Node 0 0 l' r')
            newFactor = balanceFactor newNode

-- execute a left rotation 
rotateLeft :: AVL a -> AVL a
rotateLeft (Node _ _ l (Node _ _ r1 r2)) =
    let newLeftHeight = 1 + max (height l) (height r1)
        newLeft = Node (length l + length r1) newLeftHeight l r1
        newRight = r2
        newLength = length newLeft + length newRight
        newHeight = 1 + max (height newLeft) (height newRight)
        newRoot = Node newLength newHeight newLeft newRight
    in newRoot
rotateLeft node = node

-- execute a right rotation 
rotateRight :: AVL a -> AVL a
rotateRight (Node _ _ (Node _ _ l1 l2) r) =
    let newLeft = l1
        newRightHeight = 1 + max (height l2) (height r)
        newRight = Node (length l2 + length r) newRightHeight l2 r
        newLength = length newLeft + length newRight
        newHeight = 1 + max (height newLeft) (height newRight)
        newRoot = Node newLength newHeight newLeft newRight
    in newRoot
rotateRight node = node

-- execute left-right rotation 
rotateLeftRight :: AVL a -> AVL a
rotateLeftRight (Node _ _ l r) = rotateRight $ recalc (Node 0 0 l' r)
    where l' = rotateLeft l
rotateLeftRight node = node

-- execute right-left rotation 
rotateRightLeft :: AVL a -> AVL a
rotateRightLeft (Node _ _ l r) = rotateLeft $ recalc (Node 0 0 l r')
    where r' = rotateRight r
rotateRightLeft node = node

isBalanced :: AVL a -> Bool
isBalanced (Atom _) = True
isBalanced node@(Node _ _ l r) =
    abs (balanceFactor node) <= 1 && isBalanced l && isBalanced r
