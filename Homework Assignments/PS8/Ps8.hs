module Ps8 where

import Control.Applicative
import Data.Char
import Control.Monad (guard)

import Data

import qualified Parser as P
import qualified Lexer as L
import qualified Infer as I

-- Starting production -- 

start :: P.Parser a -> P.Parser a
start p = do res <- p
             L.symbol "$"
             return res

lexp :: P.Parser Exp
lexp = labs <|> lapp

-- Lambda abstraction
labs :: P.Parser Exp
labs = do
    lambda
    var <- L.identifier
    arrow
    body <- lexp
    return (ELambda var body)

-- Application
lapp :: P.Parser Exp
lapp = do
    terms <- some lterm
    return (foldl1 EApp terms)

-- Terms
lterm :: P.Parser Exp
lterm = parenExp <|> identExp <|> intExp <|> boolExp

-- Parenthesized expression
parenExp :: P.Parser Exp
parenExp = do
    L.symbol "("
    e <- lexp
    L.symbol ")"
    return e

-- Identifier
identExp :: P.Parser Exp
identExp = do
    EVar <$> L.identifier

-- Integer literal
intExp :: P.Parser Exp
intExp = do
    EPrim . PNum <$> L.integer

-- Boolean literals 'T' and 'F'
boolExp :: P.Parser Exp
boolExp = (do
    L.symbol "T"
    return (EPrim (PBool True)))
    <|> (do
        L.symbol "F"
        return (EPrim (PBool False)))

-- Helper parsers for symbols
lambda :: P.Parser String
lambda = L.symbol "\\" <|> L.symbol "λ"

arrow :: P.Parser String
arrow = L.symbol "->"

s :: P.Parser [Int]
s = s1 `combine` s2 `combine` s3

-- Combine parsers to collect all possible parses
combine :: P.Parser a -> P.Parser a -> P.Parser a
combine pa pb = P.P (\input -> P.runParser pa input ++ P.runParser pb input)

-- Production 1: s ::= a s b s
s1 :: P.Parser [Int]
s1 = do
    L.symbol "a"
    l1 <- s
    L.symbol "b"
    l2 <- s
    return (1 : l1 ++ l2)

-- Production 2: s ::= b s a s
s2 :: P.Parser [Int]
s2 = do
    L.symbol "b"
    l1 <- s
    L.symbol "a"
    l2 <- s
    return (2 : l1 ++ l2)

-- Production 3: s ::= ε
s3 :: P.Parser [Int]
s3 = return [3]
