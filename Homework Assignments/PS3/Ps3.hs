module Ps3 where

-- This function alternates two functions as traversing the list. 
altMap :: (a -> b) -> (a -> b)-> [a] -> [b]
altMap f g [] = []
altMap f g (x:xs) = f x : altMap g f xs

{- End of Problem 1 -}

-- This function does the same as toDigit in PS1. 
-- It follows a more wholemeal programming style. 
toDigits :: Integer -> [Integer]
toDigits n = reverse . map (`mod` 10) . takeWhile (> 0) $ iterate (`div` 10) n

-- This function does the same as doubleOther in PS1. 
-- It follows a more wholemeal programming style. 
doubleOther :: [Integer] -> [Integer]
doubleOther = reverse . altMap id (* 2) . reverse

-- This function does the same as subNine in PS1. 
-- It follows a more wholemeal programming style.
subNine :: [Integer] -> [Integer]
subNine = map sub
            where sub d = if d > 9 then d - 9 else d

-- This function does the same as validate in PS1. 
-- It follows a more wholemeal programming style.
validate :: Integer -> Bool
validate = (== 0) . (`mod` 10) . sum . subNine . doubleOther . toDigits

{- End of Problem 2 -}

-- This function over-writes func1 in wholemeal style.
fun1' :: [Integer] -> Integer
fun1' = foldl minus 1
            where minus prod n = if even n then (n - 2) * prod else prod

-- This function over-writes func2 in wholemeal style.
fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/= 1) . iterate selection
            where selection n = if even n then n `div` 2 else 3 * n + 1

{- End of Problem 3 -}

-- This function coverts an integer list into its integer expression.
digitsToInt :: [Integer] -> Integer
digitsToInt = foldl (\s d -> s * 10 + d) 0

{- End of Problem 4 -}

-- This function takes the transpose of a matrix, which invokes two foldrs.
-- One iterates over each row. The other appends entries to their columns.
transpose :: [[a]] -> [[a]]
transpose = foldr (\r tran -> insertion [] (zip r (tran ++ repeat []))) []
                where insertion = foldr (\(x, c) col -> (x:c) : col)

{- End of Problem 5 -}
