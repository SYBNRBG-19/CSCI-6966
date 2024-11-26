module BufferOverflowChecker (checkBufferOverflow) where

import CFGGenerator
import Types
import BinaryParser (Instruction(..))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (isInfixOf)

-- List of unsafe functions that can cause buffer overflows
unsafeFunctions :: [String]
unsafeFunctions = ["strcpy", "gets", "scanf", "sprintf", "strcat", "strncpy", "memcpy", "memmove"]

-- Main function to check for buffer overflow vulnerabilities
checkBufferOverflow :: CFG -> [Vulnerability]
checkBufferOverflow cfg =
  concatMap checkBlockForVulnerabilities (Map.elems (cfgBlocks cfg))

-- Check a basic block for buffer overflow vulnerabilities
checkBlockForVulnerabilities :: BasicBlock -> [Vulnerability]
checkBlockForVulnerabilities bb =
  mapMaybe (checkInstructionForVulnerability bb) (bbInstructions bb)

-- Check an instruction for buffer overflow vulnerability
checkInstructionForVulnerability :: BasicBlock -> Instruction -> Maybe Vulnerability
checkInstructionForVulnerability _ instr =
  if isCallInstruction instr && isUnsafeFunction (operands instr)
     then Just Vulnerability
          { vulnType = "Buffer Overflow Vulnerability"
          , description = "Possible buffer overflow in call to " ++ extractFunctionName (operands instr)
          , location = address instr
          }
     else Nothing

-- Check if the instruction is a call instruction
isCallInstruction :: Instruction -> Bool
isCallInstruction instr = mnemonic instr `elem` ["call", "callq"]

-- Check if the called function is an unsafe function
isUnsafeFunction :: String -> Bool
isUnsafeFunction operand =
  any (`isInfixOf` operand) unsafeFunctions

-- Extract function name from operand
extractFunctionName :: String -> String
extractFunctionName operand =
  let trimmedOperand = dropWhile (== '*') operand  -- Remove leading '*'
      cleaned = if "@plt" `isInfixOf` trimmedOperand
                then takeWhile (/= '@') trimmedOperand  -- Remove '@plt' suffix
                else trimmedOperand
  in cleaned
