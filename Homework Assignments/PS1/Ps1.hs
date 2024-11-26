{-# OPTIONS_GHC -Wall #-} 

module Ps1 where

import Data.Char
 
-- This function coverts the card number into a list of digits.
toDigits :: Integer -> [Int] 
toDigits num = map (\digit -> ord digit - ord '0') (show num) 

-- This function checks if the argument is even.
isEven :: Int -> Bool
isEven a = even a

-- This function doubles every other digit in a list from L to R.
doubleLeft :: [Int] -> [Int] -> [Int]
doubleLeft = zipWith (\index x -> if isEven index then x * 2 else x)

-- This function doubles every other digit of a list using doubleLeft.
-- It starts from the second-to-last digit and moves left.
doubleOther :: [Int] -> [Int]
doubleOther xs = reverse (doubleLeft [1..] (reverse xs))

-- This function subtracts 9 from all digits >= 9 in a list.
subNine :: [Int] -> [Int] 
subNine xs = map (\digit -> if digit > 9 then digit - 9 else digit) xs

-- This function combines rule (1), (2), and (3).
sumDigits :: Integer -> Int
sumDigits num = sum (subNine (doubleOther (toDigits num)))

-- This function checks if a card # is valid using sumDigits.
validate :: Integer -> Bool
validate num | sumDigits num `mod` 10 == 0 = True
             | otherwise = False

{- This is the end of Problem 1. -}

-- This function coverts a decimal into 8-bit binary list.
toBinary :: Int -> Int -> [Int]
toBinary 0 _ = []
toBinary b decimal = let (q, r) = decimal `divMod` 2
                     in r : toBinary (b - 1) q

-- This function converts a char into binary ASCII using toBinary.
char2bin :: Char -> [Int] 
char2bin char = reverse (toBinary 8 (ord char))

-- This function coverts a binary list to decimal. 
toInt :: [Int] -> Int 
toInt = foldl (\acc bit -> acc * 2 + bit) 0 

-- This function coverts a 8-digit binary list into char.
bin2char :: [Int] -> Char 
bin2char binary = chr (toInt binary)

-- This function implements xor operation.
xor :: Int -> Int -> Int
xor a b = if a == b then 0 else 1

-- This function executes xor operation on two binaries.
xorBinaries :: Char -> Char -> [Int]
xorBinaries c1 c2 = zipWith (xor) (char2bin c1) (char2bin c2)

-- This function computes the cipher for the given str and key.
encode :: String -> String -> String 
encode str key = [bin2char (xorBinaries c1 c2) | (c1, c2) <- zip str key]

-- This function computes the str for the given cipher and key. 
decode :: String -> String -> String 
decode cipher key = [bin2char (xorBinaries c1 c2) | (c1, c2) <- zip cipher key] 

{- This is the end of Problem 2. -}
