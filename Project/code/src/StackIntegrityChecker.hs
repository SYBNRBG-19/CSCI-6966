{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-unused-local-binds #-}
module StackIntegrityChecker (checkStackIntegrity, analyzeStackImbalance) where

import CFGGenerator
import Types
import BinaryParser (Instruction(..))
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.List (foldl')
import Data.Maybe (fromMaybe)
import Data.List (isPrefixOf)

-- | Normalize labels by stripping the "0x" prefix if present.
normalizeLabel :: String -> String
normalizeLabel label = if "0x" `isPrefixOf` label then drop 2 label else label

-- | Main function to check for stack integrity vulnerabilities
checkStackIntegrity :: CFG -> [Vulnerability]
checkStackIntegrity cfg
    | not (null exitBlocks) = checkExitBlocks
    | otherwise              = checkNetStackBalance
  where
    -- Define exit mnemonics
    exitMnemonics = ["ret", "syscall"]

    -- Identify all exit blocks containing any exit mnemonic
    exitBlocks = [ bb | bb <- Map.elems (cfgBlocks cfg), hasExitInstruction exitMnemonics bb ]

    -- | Check stack balance at exit points using data-flow analysis
    checkExitBlocks =
        concatMap reportVuln retChecks
      where
        stackBalances = performDataFlowAnalysis cfg exitMnemonics
        retChecks = [ (normalizeLabel (bbLabel bb), fromMaybe Set.empty (Map.lookup (bbLabel bb) stackBalances)) | bb <- exitBlocks ]
        reportVuln (label, balances) =
            [Vulnerability
                { vulnType = "Stack Integrity Vulnerability"
                , description = "Stack is imbalanced at exit point in basic block " ++ label
                , location = label
                } | not (Set.member 0 balances)]

    -- | Check net stack balance of the entire function when there are no exit instructions
    checkNetStackBalance =
        if netBalance /= 0
            then [Vulnerability
                    { vulnType = "Stack Integrity Vulnerability"
                    , description = "Stack is imbalanced in function. Net stack balance: " ++ show netBalance
                    , location = "Function level"
                    }]
            else []
      where
        instrs = concatMap bbInstructions (Map.elems $ cfgBlocks cfg)
        netBalance = analyzeStackImbalance instrs

    -- | Check if a basic block contains any exit instruction
    hasExitInstruction :: [String] -> BasicBlock -> Bool
    hasExitInstruction exits bb = any (\instr -> mnemonic instr `elem` exits) (bbInstructions bb)

    -- | Perform data-flow analysis to compute stack balances at each block
    performDataFlowAnalysis :: CFG -> [String] -> Map.Map String (Set.Set Int)
    performDataFlowAnalysis cfg exits = iterateAnalysis initialInBalances workList
      where
        entryLabel = getEntryBlockLabel cfg
        allLabels = Map.keys (cfgBlocks cfg)
        initialInBalances = Map.fromList [ (lbl, if lbl == entryLabel then Set.singleton 0 else Set.empty) | lbl <- allLabels ]
        workList = Set.fromList allLabels

        iterateAnalysis inBalances wl
          | Set.null wl = inBalances
          | otherwise =
              let (current, restWL) = Set.deleteFindMin wl
                  currentBlock = fromMaybe (error $ "Block not found: " ++ current) (Map.lookup current (cfgBlocks cfg))
                  predecessors = getPredecessors cfg current
                  incomingBalances = if null predecessors
                                        then fromMaybe Set.empty (Map.lookup current initialInBalances)
                                        else foldl' Set.union Set.empty [ fromMaybe Set.empty (Map.lookup predLbl inBalances) | predLbl <- predecessors ]
                  newInBalances = incomingBalances
                  netEffect = computeNetStackEffect currentBlock
                  outgoingBalances = Set.map (+ netEffect) incomingBalances
                  successors = fromMaybe [] (Map.lookup current (cfgEdges cfg))
                  (inBalances', newWL) = foldl' (updateSuccessor outgoingBalances) (Map.insert current outgoingBalances inBalances, restWL) successors
              in iterateAnalysis inBalances' newWL

        -- | Update successor's incoming balances and add to worklist if changed
        updateSuccessor :: Set.Set Int -> (Map.Map String (Set.Set Int), Set.Set String) -> String -> (Map.Map String (Set.Set Int), Set.Set String)
        updateSuccessor outBalances (accBalances, accWL) succLbl =
            let normalizedSuccLbl = normalizeLabel succLbl
                existing = fromMaybe Set.empty (Map.lookup succLbl accBalances)
                updated = Set.union existing outBalances
            in if updated /= existing
                then (Map.insert succLbl updated accBalances, Set.insert succLbl accWL)
                else (accBalances, accWL)

        -- | Get the label of the entry block (assumed to be the first block)
        getEntryBlockLabel :: CFG -> String
        getEntryBlockLabel cfg =
            case Map.keys (cfgBlocks cfg) of
                (lbl:_) -> lbl
                []      -> error "CFG has no blocks"

        -- | Get predecessors of a block by traversing cfgEdges
        getPredecessors :: CFG -> String -> [String]
        getPredecessors cfg target =
            [ lbl | (lbl, succs) <- Map.toList (cfgEdges cfg), target `elem` succs ]

        -- | Compute the net stack effect of a basic block
        computeNetStackEffect :: BasicBlock -> Int
        computeNetStackEffect bb = sum $ map stackEffect (bbInstructions bb)

-- | Calculate the net stack imbalance of a list of instructions
analyzeStackImbalance :: [Instruction] -> Int
analyzeStackImbalance = foldl' (\acc instr -> acc + stackEffect instr) 0

-- | Estimate the stack effect of an instruction
stackEffect :: Instruction -> Int
stackEffect instr =
    case mnemonic instr of
        "push"    -> 1
        "pop"     -> -1
        "call"    -> -1
        "ret"     -> 1
        "syscall" -> 0   -- Assuming syscall does not affect stack directly
        "enter"   -> -1
        "leave"   -> 1
        _         -> 0