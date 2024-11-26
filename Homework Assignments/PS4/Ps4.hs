{-# OPTIONS_GHC -Wall #-}

module Ps4 where

import Data.Char

-- Problem 1. Expression interpretation

data ExprT = Val Int
           | Add ExprT ExprT
           | Mul ExprT ExprT
           deriving (Eq)

instance Show ExprT where
  -- Implementation of show function using prettyPrint'.
  -- show :: ExprT -> String
  show = prettyPrint'

-- This function captures and reuses common traversal functionality.
myFold :: (Int -> a) -> (a -> a -> a) -> (a -> a -> a) -> ExprT -> a
myFold fVal fAdd fMul e = case e of
    Val n -> fVal n
    Add x y -> fAdd (myFold fVal fAdd fMul x) (myFold fVal fAdd fMul y)
    Mul x y -> fMul (myFold fVal fAdd fMul x) (myFold fVal fAdd fMul y)

-- This function evalutes the expression and outputs an integer.
eval :: ExprT -> Int
eval = myFold id (+) (*)

-- This function returns a string representing the expression. 
-- All parentheses are included in the string. 
prettyPrint :: ExprT -> String
prettyPrint = myFold show printAdd printMul
    where
        printAdd x y = "(" ++ x ++ " + " ++ y ++ ")"
        printMul x y = "(" ++ x ++ " * " ++ y ++ ")"

-- This function does the same thing as previous one.
-- However, it removes all redundant parentheses. 
prettyPrint' :: ExprT -> String
prettyPrint' = myFold show printAdd' printMul'
  where
    printAdd' x y
        | '+' `elem` y = x ++ " + (" ++ y ++ ")"
        | otherwise = x ++ " + " ++ y

    printMul' x y 
        | '+' `elem` x && '+' `elem` y = "(" ++ x ++ ") * (" ++ y ++ ")" 
        | '+' `elem` x = "(" ++ x ++ ") * " ++ y
        | '+' `elem` y = x ++ " * (" ++ y ++ ")"
        | otherwise = x ++ " * " ++ y

-- Problem 2. LogMessage parse

-- | @runParse f n fp@ tests the log file parser @f@ by running it 
--   on the log file @fp@ and displaying @n@ results.
runParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
runParse f n fp = (readFile fp) >>= mBind
                where mBind bigStr = return (take n $ f bigStr)

data MessageType = Info
                | Warning
                | Error Level
                deriving (Show,Eq)

data LogMessage = LogMessage {
    mType :: MessageType, timeStamp :: TimeStamp, message :: String
} deriving (Show,Eq)

newtype Level = Level Int deriving (Eq, Show)

newtype TimeStamp = TimeStamp Int deriving (Eq, Show)

class ParseRecord a where
  parseRecord :: [String] -> Maybe a

class ParseField a where
  parseField :: String -> Maybe a

instance ParseField Level where
  -- Implementation of safe reading operation.
  -- parseField :: String -> Maybe Level
  parseField str = case reads str of 
    [(t, "")] -> Just (Level t)
    _ -> Nothing 

instance ParseField TimeStamp where
  -- Implementation of safe reading operation.
  -- parseField :: String -> Maybe TimeStamp
  parseField str = case reads str of 
    [(t, "")] -> Just (TimeStamp t)
    _ -> Nothing 

instance ParseRecord LogMessage where
  -- Implementation of parsing a record to LogMessage using safe reading.
  -- parseRecord :: [String] -> Maybe LogMessage
  parseRecord xss = case xss of
    ("I" : t : m) -> case parseField t of 
        Just (TimeStamp i) -> Just $ LogMessage Info (TimeStamp i) (unwords m)
        _ -> Nothing
    ("W" : t : m) -> case parseField t of  
        Just (TimeStamp i) -> Just $ LogMessage Warning (TimeStamp i) msg
            where msg = unwords m
        _ -> Nothing 
    ("E" : l : t : m) -> case (parseField l, parseField t) of 
        (Just (Level s), Just (TimeStamp i)) -> Just $ LogMessage sv stamp msg
            where 
                sv = Error (Level s)
                stamp = TimeStamp i
                msg = unwords m
        _ -> Nothing
    _ -> Nothing

-- This function invokes the previous parseRecord to parse a row. 
parseMessage :: String -> Maybe LogMessage
parseMessage s = parseRecord . words $ s

-- This function calls the previous parseMessage to parse all rows in a file.
parse :: String -> [LogMessage]
parse file = foldr (\x acc -> case parseMessage x of
    Nothing -> acc
    Just y -> y : acc) [] (lines file)
