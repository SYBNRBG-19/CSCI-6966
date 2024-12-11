-- FormatStringChecker.hs
module FormatStringChecker (checkFormatStringVulnerabilities) where

import CFGGenerator
import Types
import BinaryParser (Instruction(..))
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.List (isSuffixOf)

-- | List of formatted output functions to check (base names)
formattedOutputFunctions :: [String]
formattedOutputFunctions = ["printf", "sprintf", "fprintf", "vprintf", "vsprintf", "snprintf", "vfprintf"]

-- | Internal mapping from function addresses to function names.
-- This mapping should be updated based on the binary being analyzed.
-- For demonstration, we include a single entry for printf@plt.
functionAddressMap :: Map.Map String String
functionAddressMap = Map.fromList [
    ("401010", "printf@plt")
    ]

-- | Check for format string vulnerabilities.
-- Accepts a CFG and returns a list of Vulnerability reports.
checkFormatStringVulnerabilities :: CFG -> [Vulnerability]
checkFormatStringVulnerabilities cfg =
  concatMap (checkBlockForVulnerabilities) (Map.elems (cfgBlocks cfg))
  where
    checkBlockForVulnerabilities :: BasicBlock -> [Vulnerability]
    checkBlockForVulnerabilities bb =
      mapMaybe (checkInstructionForVulnerability) (bbInstructions bb)
    
    -- | Check an instruction for format string vulnerability.
    checkInstructionForVulnerability :: Instruction -> Maybe Vulnerability
    checkInstructionForVulnerability instr =
      case mnemonic instr of
        "call" ->
          let operand = operands instr
              -- Lookup the function name based on the operand address
              funcNameWithSuffix = Map.lookup operand functionAddressMap
              -- Extract the base function name (e.g., "printf" from "printf@plt")
              funcName = case funcNameWithSuffix of
                          Just name -> extractBaseFunctionName name
                          Nothing -> Nothing
          in case funcName of
               Just name | name `elem` formattedOutputFunctions ->
                 Just Vulnerability
                   { vulnType = "Format String Vulnerability"
                   , description = "Possible format string vulnerability in call to " ++ name
                   , location = address instr
                   }
               _ -> Nothing
        _ -> Nothing

    -- | Extract base function name by removing suffixes like '@plt'.
    extractBaseFunctionName :: String -> Maybe String
    extractBaseFunctionName name =
      if "@plt" `isSuffixOf` name
        then Just (take (length name - length "@plt") name)
        else Just name
