{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE FlexibleContexts #-}

module Quiz5 where

import Data.Foldable
import Text.Parsec
import qualified Text.Parsec.Token as P
import Text.Parsec.Language (emptyDef)

import Test.QuickCheck (Gen, Property, Arbitrary(..), Testable(..), (==>))
import qualified Test.QuickCheck as QC
import Control.Monad (liftM,liftM2,liftM3,replicateM)

data NL a = Atom a
          | List [NL a]
          deriving (Show,Eq)

instance Semigroup (NL a) where 
    -- <> :: NL a -> NL a -> NL a
    nl1 <> nl2 = List (lift nl1 ++ lift nl2) where
        lift (Atom x) = [Atom x]
        lift (List xs) = xs

instance Monoid (NL a) where
    -- mempty :: NL a
    mempty = List []

nl1 :: NL Int
nl1 = List [Atom 1, Atom 2, List [Atom 3]]

nl2 :: NL Int
nl2 = List [List [Atom 1]]

empty :: NL Int
empty = List []

-- Question 1: Instantiate NL as Foldable. 

instance Foldable NL where
    -- foldMap :: (a->b) -> NL a -> NL b
    foldMap f (Atom x) = f x
    foldMap f (List xs) = foldMap (foldMap f) xs

--- Parsers, using Parsec ---

langDef = emptyDef { P.identStart = lower
                   , P.identLetter = alphaNum
                   }

-- lexer :: P.GenTokenParser String u Identity
lexer       = P.makeTokenParser langDef

-- The two token parsers necessary to parse a list of integers

integer :: Parsec String u Integer
integer = P.integer lexer
symbol :: String -> Parsec String u String
symbol = P.symbol lexer

--- Question 2. Use parsec to define a parser for NL (grammar is given)
nl :: Parsec String u (NL Integer)
nl = (Atom <$> integer)
    <|> List <$> brackets nlList
    where
      brackets p = symbol "[" *> p <* symbol "]"
      nlList = nl `sepBy` symbol ","

-- runParser nl () "" "[1,2,[3]]"

-- Question 3. Use Parsec to construct an NL (Integer,Integer) with depth. 

nl' :: Parsec String Integer (NL (Integer,Integer))
nl' = atom' <|> list' where
    atom' = do
      d <- getState
      x <- integer
      return (Atom (x, d)) 

    list' = do
      d <- getState
      symbol "["
      putState (d+1)     
      xs <- nlList'
      symbol "]"
      putState d       
      return (List xs)

    nlList' = nl' `sepBy` symbol ","

-- runParser nl' 0 "" "[1,2,[3]]"

-- QuickCheck generators --

-- Question 4. Generator and instance of Arbitrary to generate random NL trees

genNL :: forall a. (Arbitrary a) => Gen (NL a)
genNL = QC.sized gen where 
  gen n | n <= 1 = Atom <$> arbitrary
        | otherwise = do
            isAtom <- QC.elements [True, False]
            if isAtom
            then Atom <$> arbitrary
            else do
              k <- QC.choose (0, max 0 (n-1))
              let subSize = n `div` (if k == 0 then 1 else k+1)
              xs <- replicateM k (gen subSize)
              return (List xs) 

instance (Arbitrary a) => Arbitrary (NL a) where
    arbitrary = genNL

-- map length <$> QC.sample' (genNL :: Gen (NL Int)) 

-- Propererties --

-- Question 5. Define QC properties to test the monoid properties. Note that 
-- we aren't concerened with the structure of the list, only the linearization of it

prop_assoc :: (NL Integer) -> (NL Integer) -> (NL Integer) -> Bool 
prop_assoc xs ys zs = toList (xs <> (ys <> zs)) == toList ((xs <> ys) <> zs)

prop_ident :: (NL Integer) -> Bool 
prop_ident xs = toList (xs <> mempty) == toList xs &&
                toList (mempty <> xs) == toList xs

-- QC.quickCheck prop_assoc