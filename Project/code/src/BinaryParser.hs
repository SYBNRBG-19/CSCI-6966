module BinaryParser (parseBinary, Instruction(..), parseObjdumpOutput, isHexDigit) where

import System.Process (readProcess)
import Data.Maybe (mapMaybe)
import Data.Char (toLower)
import Data.List (isSuffixOf, findIndex, isPrefixOf)
import Numeric ()
import Debug.Trace (trace)  -- Added import for trace

data Instruction = Instruction
  { address   :: String
  , bytes     :: String
  , mnemonic  :: String
  , operands  :: String
  } deriving (Show, Eq)

-- Comprehensive list of x86 base mnemonics (extend as needed)
knownMnemonics :: [String]
knownMnemonics = 
  [ "add", "sub", "mov", "push", "pop", "ret", "jmp", "je", "jne"
  , "jg", "jge", "jl", "jle", "ja", "jb", "call", "int", "nop"
  , "lea", "cmp", "test", "inc", "dec", "xor", "and", "or", "not"
  , "sbb", "adc", "mul", "div", "loop", "cmov", "syscall"
  -- Add more mnemonics as necessary
  ]

-- List of possible suffixes indicating operand sizes
sizeSuffixes :: [String]
sizeSuffixes = ["l", "w", "b", "q"]

-- Function to strip size suffixes from a mnemonic only if the base mnemonic is known
stripSuffix :: String -> String
stripSuffix mnem =
  let possibleBases = [take (length mnem - length s) mnem | s <- sizeSuffixes, s `isSuffixOf` mnem]
      validBases = filter (`elem` knownMnemonics) possibleBases
  in if not (null validBases)
       then head validBases
       else mnem

-- Function to parse the binary using objdump
parseBinary :: FilePath -> IO [Instruction]
parseBinary path = do
  output <- readProcess "objdump" ["-d", path] ""
  return $ parseObjdumpOutput output

-- Function to parse the objdump output into a list of Instructions
parseObjdumpOutput :: String -> [Instruction]
parseObjdumpOutput output = mapMaybe parseLine textSectionLines
  where
    allLines = lines output

    -- Extract lines within the .text section
    textSectionLines = extractTextSection allLines

    -- Helper function to extract lines after "Disassembly of section .text:"
    extractTextSection :: [String] -> [String]
    extractTextSection [] = []
    extractTextSection (x:xs)
      | "Disassembly of section .text:" `isPrefixOf` x = extractInstructions xs
      | otherwise = extractTextSection xs

    -- Extract instructions until the next "Disassembly of section" or end of file
    extractInstructions :: [String] -> [String]
    extractInstructions [] = []
    extractInstructions (y:ys)
      | "Disassembly of section " `isPrefixOf` y = []
      | otherwise = y : extractInstructions ys

    parseLine :: String -> Maybe Instruction
    parseLine line =
      case words line of
        (addr:rest)
          | ':' `elem` addr && all isHexDigit (takeWhile (/= ':') addr) ->
              parseInstruction rest (init addr)
        _ -> Nothing

    parseInstruction :: [String] -> String -> Maybe Instruction
    parseInstruction tokens addr =
      case findMnemonicIndex tokens of
        Just i ->
          let byteTokens = take i tokens
              byteCodes = unwords byteTokens
              mnemToken = tokens !! i
              operandsTokens = drop (i + 1) tokens
              operandsCombined = unwords operandsTokens
              operandsClean = takeWhile (/= '#') operandsCombined -- Remove comments
              operandsFinal = case words (trim operandsClean) of
                               (w:_) -> w
                               []    -> ""
              mnemLower = map toLower mnemToken
              baseMnem = stripSuffix mnemLower
              -- Debugging Statement
              _ = trace ("Parsed Instruction - Address: " ++ addr ++ ", Mnemonic: " ++ baseMnem ++ ", Operands Final: " ++ operandsFinal) ()
          in if baseMnem `elem` knownMnemonics
             then Just $ Instruction addr byteCodes baseMnem operandsFinal
             else Nothing -- Unknown mnemonic encountered
        Nothing -> Nothing -- No known mnemonic found

    -- Function to find the index of the first known mnemonic in the tokens
    findMnemonicIndex :: [String] -> Maybe Int
    findMnemonicIndex tokens = findIndex isMnemonic tokens

    -- Function to check if a token is a known mnemonic (after stripping suffix)
    isMnemonic :: String -> Bool
    isMnemonic token =
      let mnemLower = map toLower token
          baseMnem = stripSuffix mnemLower
      in baseMnem `elem` knownMnemonics

    -- Function to trim leading and trailing whitespace
    trim :: String -> String
    trim = f . f
       where f = reverse . dropWhile (`elem` [' ', '\t'])

-- Helper function to check if a character is a hexadecimal digit
isHexDigit :: Char -> Bool
isHexDigit c = c `elem` "0123456789abcdefABCDEF"
