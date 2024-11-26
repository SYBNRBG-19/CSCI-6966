module Lexer (identifier, integer, natural, symbol) where

import Control.Applicative
import Data.Char

import qualified Parser as P

-- Lexer primitives --

{-
item :: Parser Char 
item = P (\s -> case s of 
    [] -> []
    (x:xs) -> [(x,xs)])
-}

sat :: (Char -> Bool) -> P.Parser Char
sat p = do x <- P.item
           if (p x) then return x else empty

digit :: P.Parser Char
digit = sat isDigit

lower :: P.Parser Char
lower = sat isLower

upper :: P.Parser Char
upper = sat isUpper

alphanum :: P.Parser Char
alphanum = sat isAlphaNum

char :: Char -> P.Parser Char
char x = sat (== x)

-- matching string
string :: String -> P.Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

ident :: P.Parser String
ident = do x <- lower -- parse first char as lower-case char
           xs <- many alphanum -- parse alphanum*
           return (x:xs)

nat :: P.Parser Int 
nat = do xs <- some digit -- (digit)^+
         return (read xs) -- converting to Int

int :: P.Parser Int
int = do sign <- (char '-') 
         n <- nat
         return (-n)
       <|> nat   

space :: P.Parser () -- returns nothing
space = do many (sat isSpace) 
           return ()

token :: P.Parser a -> P.Parser a 
token p = do space
             v <- p
             -- space
             return v

identifier :: P.Parser String
identifier = token ident

natural :: P.Parser Int
natural = token nat

integer :: P.Parser Int
integer = token int

symbol :: String -> P.Parser String
symbol xs = token (string xs)