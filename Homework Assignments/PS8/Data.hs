{-# OPTIONS_GHC -Wall #-}

module Data
  ( Ident, TVar
  , Prim(..), Exp(..)
  , BaseType(..), Type(..)
  , TCError (..)
  , Env, TEnv
  , Subst, idSubst
  , intType, boolType )
  where
import Data.Typeable
import Control.Exception

type Ident = String
type TVar = String

-- Language.
data Prim =
    PNum Int
  | PBool Bool
  deriving (Eq,Show)

data Exp =
    EVar Ident
  | ELambda Ident Exp
  | EApp Exp Exp
  | EPrim Prim
  deriving (Eq,Show)

-- Types.
data BaseType = BTInt | BTBool
  deriving (Eq,Show)

data Type =
    TBase BaseType
  | TVar TVar
  | TArrow Type Type
  deriving (Eq,Show)

-- Errors.
data TCError =
    UnboundVar String
  | TypeCircularity
  | TypeMismatch Type Type
  | Default String
  deriving (Show, Typeable)
instance Exception TCError

-- Environment.
type Env = [(String, Exp)]
type TEnv = [(String, Type)]

-- Substitution environment.
type Subst = [(TVar, Type)]

idSubst :: [a]
idSubst = []

-- Default types.
intType::Type
intType = TBase BTInt
boolType :: Type
boolType = TBase BTBool
