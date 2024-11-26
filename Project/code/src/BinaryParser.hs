module BinaryParser (parseBinary, Instruction(..), parseObjdumpOutput) where

import System.Process
import Data.Maybe (mapMaybe)

data Instruction = Instruction
  { address   :: String
  , bytes     :: String
  , mnemonic  :: String
  , operands  :: String
  } deriving (Show, Eq)

parseBinary :: FilePath -> IO [Instruction]
parseBinary path = do
  output <- readProcess "objdump" ["-d", path] ""
  return $ parseObjdumpOutput output

parseObjdumpOutput :: String -> [Instruction]
parseObjdumpOutput output = mapMaybe parseLine (lines output)
  where
    parseLine line =
      case words line of
        (addr:_)
          | ':' `elem` addr && all (`elem` "0123456789abcdef") (takeWhile (/= ':') addr) ->
              parseInstruction line
        _ -> Nothing

parseInstruction :: String -> Maybe Instruction
parseInstruction line =
  let parts = words line
      addr = head parts
      byteCodes = unwords $ takeWhile (all (`elem` "0123456789abcdef")) (tail parts)
      mnemonicOperands = dropWhile (all (`elem` "0123456789abcdef")) (tail parts)
  in case mnemonicOperands of
       (rMnemonic:rOperands) -> Just $ Instruction (init addr) byteCodes rMnemonic (unwords rOperands)
       _ -> Nothing
