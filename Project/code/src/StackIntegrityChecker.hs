module StackIntegrityChecker (checkStackIntegrity, analyzeStackImbalance) where

import CFGGenerator
import Types
import BinaryParser (Instruction(..))
import qualified Data.Map as Map
import Data.List (foldl')
-- import Debug.Trace

-- Main function to check for stack integrity vulnerabilities
checkStackIntegrity :: CFG -> [Vulnerability]
checkStackIntegrity cfg =
  concatMap checkBlockForVulnerabilities (Map.elems (cfgBlocks cfg))

-- Check a basic block for stack integrity vulnerabilities
checkBlockForVulnerabilities :: BasicBlock -> [Vulnerability]
checkBlockForVulnerabilities bb =
  let imbalance = analyzeStackImbalance (bbInstructions bb)
  in ([Vulnerability
                { vulnType = "Stack Integrity Vulnerability"
                , description = "Possible stack imbalance detected in basic block " ++ bbLabel bb
                , location = bbLabel bb
                } | imbalance /= 0])

-- Analyze the stack balance in a list of instructions
analyzeStackImbalance :: [Instruction] -> Int
analyzeStackImbalance = foldl' (\acc instr ->
    let effect = stackEffect instr
    in
      -- trace ("Instruction: " ++ show (mnemonic instr) ++ " | Effect: " ++ show effect ++ " | Accumulator: " ++ show (acc + effect))
      (acc + effect)
  ) 0

-- Estimate the stack effect of an instruction
stackEffect :: Instruction -> Int
stackEffect instr =
  case mnemonic instr of
    "push"  -> 1
    "pop"   -> -1
    "call"  -> -1
    "ret"   -> 1
    "enter" -> -1
    "leave" -> 1
    _       ->
      -- trace ("Unhandled instruction: " ++ show (mnemonic instr))
      0
