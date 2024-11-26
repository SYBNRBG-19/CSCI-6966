{-# OPTIONS_GHC -Wall #-}

module Ps5Connections where

import System.IO
import Data.List

connections :: IO ()
connections = do
    putStrLn "Enter the file name: "
    -- Get file name and read the file.
    file <- getLine
    cats <- readPuzzle file
    -- Start the game with 4 mistakes allowed.
    game cats (concatMap cWords cats) 4 []

-- Data type to store a category and its words.
data Cat = Cat { cName :: String, cWords :: [String] } deriving (Show, Eq)

-- Read the puzzle file into the Category type. 
readPuzzle :: FilePath -> IO [Cat]
readPuzzle file = do
    contents <- readFile file
    return $ map parse (lines contents)
    where
        -- Parse a single category. 
        parse :: String -> Cat
        parse str =
            let (_catCode, rest1) = span (/= ':') str
                rest2 = drop 1 rest1
                ws = words rest2
            in case ws of
                (catName:wordList) -> Cat catName wordList
                _ -> error "Invalid puzzle format"

-- Game body. 
game :: [Cat] -> [String] -> Int -> [Cat] -> IO ()
game cats rmWs mistakes crtGs = do
    -- Output current state.
    putStrLn "Correct guesses: "
    let crtGsList = map cName crtGs
    if not (null crtGsList)
        then printStrings 3 crtGsList
        else putStrLn "None"
    putStrLn "Remaining words: "
    let rmWsList = sort rmWs
    if not (null rmWsList)
        then printStrings 4 rmWsList
        else putStrLn "None"
    putStrLn $ "Remaining mistakes: " ++ show mistakes ++ "\n"
    -- check if the game ends. 
    if null rmWs
        then putStrLn "Player 2 wins."
    else if mistakes == 0
            then putStrLn "Player 1 wins."
        else do
            -- Game continues. Prompt player 2 to enter a new guess.
            putStrLn "Make four guesses: "
            w1 <- getLine
            w2 <- getLine
            w3 <- getLine
            w4 <- getLine
            let guesses = [w1, w2, w3, w4]
            -- Check guesses and update remaining words. 
            let remainingCats = cats
            let (crtGs', rmWs') = checkGs guesses remainingCats crtGs rmWs
            -- If the guess is wrong, reduce the mistakes by 1. 
            -- Otherwise, update correct guesses. 
            if crtGs' == crtGs
                then game cats rmWs (mistakes - 1) crtGs
            else
                game cats rmWs' mistakes crtGs'

-- Check if the given guess is correct. 
-- It returns the updated correct guesses and remaining words.
checkGs :: [String] -> [Cat] -> [Cat] -> [String] -> ([Cat], [String])
checkGs guesses cats crtGs rmWs =
    case [cat | cat <- cats, sort (cWords cat) == sort guesses] of
        [] -> (crtGs, rmWs)
        (cat:_) -> (cat : crtGs, rmWs \\ guesses)

-- Function to split a list into chunks of a given size
chunkList :: Int -> [a] -> [[a]]
chunkList _ [] = []
chunkList n xs = take n xs : chunkList n (drop n xs)

-- Function to print a list of strings, with a certain number of strings per line
printStrings :: Int -> [String] -> IO ()
printStrings n = mapM_ (putStrLn . intercalate ", ") . chunkList n
