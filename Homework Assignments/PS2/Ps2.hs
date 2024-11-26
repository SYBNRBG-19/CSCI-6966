{-# OPTIONS_GHC -Wall #-}

module Ps2 where

import Data.Char

data MessageType = Info
                | Warning
                | Error Int
                deriving (Show, Eq)

data LogMessage = LogMessage MessageType Int String
                | Unknown String
                deriving (Show, Eq)

-- | @runParse f n fp@ tests the log file parser @f@ by running it 
--   on the log file @fp@ and displaying @n@ results.
runParse :: (String -> [LogMessage])
          -> Int
          -> FilePath
          -> IO [LogMessage]
runParse f n fp = (readFile fp) >>= mBind
                where mBind bigStr = return (take n $ f bigStr)

-- This function verifies if the argument is integer. 
safeRead :: String -> Maybe Int
safeRead str = case reads str of
                [(mid, "")] -> Just mid 
                _ -> Nothing 

-- This function converts a string into a LogMessage based on its type. 
-- Unkown types will be added as Unknown. 
parseMessage :: String -> LogMessage
parseMessage str = case words str of
                ("I" : i : msg) -> case safeRead i of 
                    Just mid -> LogMessage Info mid (unwords msg)
                    Nothing -> Unknown str
                ("W" : i : msg) -> case safeRead i of  
                    Just mid -> LogMessage Warning mid (unwords msg)
                    Nothing -> Unknown str 
                ("E" : l : i : m) -> case (safeRead l, safeRead i) of 
                    (Just s, Just t) -> LogMessage (Error s) t (unwords m)
                    _ -> Unknown str
                _ -> Unknown str

-- This function takes a str list as input annd returns a LogMessage list.
-- It invokes the parseMessage function defined above. 
parse :: String -> [LogMessage]
parse strs = map parseMessage (lines strs)

-- This function checks if a msg is of Unknown type.
checkUnknown :: LogMessage -> Bool 
checkUnknown msg = case msg of
                    (Unknown _) -> True
                    _ -> False 

-- This function compares m1 with m2. 
-- If m1 <= m2, it returns True, else False. 
compareMessages :: LogMessage -> LogMessage -> Bool 
compareMessages (LogMessage _ t1 _) (LogMessage _ t2 _) = t1 <= t2
compareMessages _ _ = False

-- This function inserts a message into the sorted message list. 
-- It ignores Unknown messages. 
insert :: [LogMessage] -> LogMessage -> [LogMessage]
insert [] msg = [msg]
insert (m:ms) msg
                | checkUnknown msg = m : ms
                | compareMessages m msg = m : insert ms msg
                | otherwise = msg : m : ms

-- This function uses insert function to sort the msg list.
insertionSort :: [LogMessage] -> [LogMessage]
insertionSort [] = []
insertionSort (m:ms) = insert (insertionSort ms) m 

-- This function selects msgs with higher severity values. 
severeErrors :: Int -> [LogMessage] -> [LogMessage]
severeErrors level = filter (\m -> case m of
                                LogMessage (Error l) _ _ -> l > level
                                _ -> False)
