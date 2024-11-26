{-# OPTIONS_GHC -Wall #-}

module Parser where 

import Control.Applicative
import Data.Char
import Control.Monad (guard)

newtype Parser a = P (String -> [(a,String)])

runParser :: Parser a -> String -> [(a,String)]
runParser (P f) s = f s 

instance Functor Parser where
    -- fmap :: (a -> b) -> (Parser a) -> (Parser b)
    fmap f p = P (\s -> let xs = runParser p s in 
                          [ (f a,str) | (a,str) <- xs ] )

instance Applicative Parser where
    -- pure :: a -> Parser a
    pure x = P (\s -> [(x,s)])

    -- <*> :: Parser (a->b) -> Parser a -> Parser b
    pab <*> pa = P (\s -> let xs = runParser pab s in
                            [(f a, out2) | (f, out1) <- xs, (a, out2) <- runParser pa out1])
                            --[] -> []
                            --[(f,out1)] -> case runParser pa out1 of
                            --              [] -> []
                            --              [(a,out2)] -> [(f a, out2)] )

instance Monad Parser where
    -- return :: a -> Parser a
    return = pure

    -- >>= :: Parser a -> (a -> Parser b) -> Parser b   
    pa >>= f = P (\s -> let xs = runParser pa s in
                            [ (b,out2) | (a,out) <- xs, (b,out2) <- runParser (f a) out ])     

instance Alternative Parser where
    -- empty :: Parser a
    empty = P (\s -> [])

    -- (<|>) :: Parser a -> Parser a -> Parser a
    pa <|> pb = P (\s -> case runParser pa s of
                            [] -> runParser pb s
                            xs -> xs)

item :: Parser Char 
item = P (\s -> case s of 
    [] -> []
    (x:xs) -> [(x,xs)])