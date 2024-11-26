module FormatStringChecker (checkFormatStringVulnerabilities) where

import CFGGenerator
import Types
import BinaryParser (Instruction(..))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (isInfixOf)

-- List of formatted output functions to check
formattedOutputFunctions :: [String]
formattedOutputFunctions = ["printf", "sprintf", "fprintf", "vprintf", "vsprintf", "snprintf", "vfprintf"]

-- Main function to check for format string vulnerabilities
checkFormatStringVulnerabilities :: CFG -> [Vulnerability]
checkFormatStringVulnerabilities cfg =
  concatMap checkBlockForVulnerabilities (Map.elems (cfgBlocks cfg))

-- Check a basic block for format string vulnerabilities
checkBlockForVulnerabilities :: BasicBlock -> [Vulnerability]
checkBlockForVulnerabilities bb =
  mapMaybe (checkInstructionForVulnerability bb) (bbInstructions bb)

-- Check an instruction for format string vulnerability
checkInstructionForVulnerability :: BasicBlock -> Instruction -> Maybe Vulnerability
checkInstructionForVulnerability _ instr =
  if isCallInstruction instr && isFormattedOutputFunction (operands instr)
     then Just Vulnerability
          { vulnType = "Format String Vulnerability"
          , description = "Possible format string vulnerability in call to " ++ extractFunctionName (operands instr)
          , location = address instr
          }
     else Nothing

-- Check if the instruction is a call instruction
isCallInstruction :: Instruction -> Bool
isCallInstruction instr = mnemonic instr `elem` ["call", "callq"]

-- Check if the called function is a formatted output function
isFormattedOutputFunction :: String -> Bool
isFormattedOutputFunction operand =
  any (`isInfixOf` operand) formattedOutputFunctions

-- Extract function name from operand
extractFunctionName :: String -> String
extractFunctionName operand =
  let trimmedOperand = dropWhile (== '*') operand  -- Remove leading '*'
      cleaned = if "@plt" `isInfixOf` trimmedOperand
                then takeWhile (/= '@') trimmedOperand  -- Remove '@plt' suffix
                else trimmedOperand
  in cleaned
