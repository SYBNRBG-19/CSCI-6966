module CFGGenerator (generateCFG, CFG(..), BasicBlock(..)) where

import BinaryParser (Instruction(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, maybeToList)
import Numeric (readHex, showHex)
import Data.List (sort)
-- import Debug.Trace (trace)

-- Data type for a Basic Block
data BasicBlock = BasicBlock
  { bbLabel        :: String
  , bbInstructions :: [Instruction]
  } deriving (Show, Eq)

-- Data type for the Control Flow Graph
data CFG = CFG
  { cfgBlocks :: Map String BasicBlock
  , cfgEdges  :: Map String [String]  -- Edges from a block to its successors
  } deriving (Show, Eq)

-- Main function to generate the CFG from a list of Instructions
generateCFG :: [Instruction] -> CFG
generateCFG instructions = CFG blocks edges
  where
    -- Parse addresses to Integer for easier processing
    instrAddrs = map (\instr -> (instr, parseAddress (address instr))) instructions

    -- Collect basic block starting addresses
    bbStarts = collectBasicBlockStarts instrAddrs

    -- Build basic blocks
    blocks = buildBasicBlocks instrAddrs bbStarts

    -- Build edges between basic blocks
    edges = buildEdges blocks

-- Function to parse hexadecimal addresses
parseAddress :: String -> Integer
parseAddress s = case readHex s of
                   [(addr, "")] -> addr
                   _            -> error $ "Invalid address: " ++ s

-- Collect starting addresses of basic blocks
collectBasicBlockStarts :: [(Instruction, Integer)] -> Set Integer
collectBasicBlockStarts instrAddrs = 
  let starts = Set.fromList $ startAddr : targetAddrs ++ fallThroughAddrs
  in
    -- trace ("Basic block starts: " ++ show starts)
    starts
  where
    startAddr = snd (head instrAddrs)
    targetAddrs = mapMaybe getJumpTarget instrAddrs
    fallThroughAddrs = mapMaybe getFallThroughAddress (zip instrAddrs (tail instrAddrs))

    -- Extract jump and call targets from instructions
    getJumpTarget (instr, _) =
      if mnemonic instr `elem` jumpMnemonics
        then parseOperandAddress (operands instr)
        else Nothing
      where
        jumpMnemonics = ["jmp", "je", "jne", "jg", "jge", "jl", "jle", "ja", "jb", "call"]

    -- Extract fall-through addresses for conditional jumps and calls
    getFallThroughAddress ((instr, _), (_, nextAddr)) =
      if mnemonic instr `elem` mayFallThroughMnemonics
        then Just nextAddr
        else Nothing
      where
        mayFallThroughMnemonics = ["je", "jne", "jg", "jge", "jl", "jle", "ja", "jb", "call"]

-- Parse operand to extract the target address
parseOperandAddress :: String -> Maybe Integer
parseOperandAddress operand =
  let operand' = dropWhile (== '*') $ filter (/= ',') operand  -- Remove '*' and ','
      hexStr = case dropWhile (/= '0') operand' of
                 ('0':'x':rest) -> "0x" ++ takeWhile (`elem` "0123456789abcdefABCDEF") rest
                 _              -> ""
  in if not (null hexStr)
     then case readHex (drop 2 hexStr) of
            [(addr, "")] -> Just addr
            _            -> Nothing
     else Nothing

-- Build basic blocks from the instruction list
buildBasicBlocks :: [(Instruction, Integer)] -> Set Integer -> Map String BasicBlock
buildBasicBlocks instrAddrs bbStarts = Map.fromList $ map (\bb -> (bbLabel bb, bb)) basicBlocks
  where
    basicBlocks = extractBasicBlocks instrAddrs bbStarts

-- Extract basic blocks from instructions
extractBasicBlocks :: [(Instruction, Integer)] -> Set Integer -> [BasicBlock]
extractBasicBlocks [] _ = []
extractBasicBlocks instrAddrs bbStarts = extractBB instrAddrs
  where
    extractBB [] = []
    extractBB ((instr, addr):rest)
      | Set.member addr bbStarts =
          let (bbInstrs, remainingInstrs) = collectBBInstructions ((instr, addr):rest) addr
              bb = BasicBlock { bbLabel = showHex addr "", bbInstructions = map fst bbInstrs }
          in bb : extractBB remainingInstrs
      | otherwise = extractBB rest

    collectBBInstructions [] _ = ([], [])
    collectBBInstructions ((instr, addr):rest) startAddr
      | addr /= startAddr && Set.member addr bbStarts = ([], (instr, addr):rest)
      | mnemonic instr `elem` endMnemonics = ([(instr, addr)], rest)
      | otherwise =
          let (instrs, remaining) = collectBBInstructions rest startAddr
          in ((instr, addr):instrs, remaining)
      where
        endMnemonics = ["jmp", "ret"]

-- Build edges between basic blocks based on control flow
buildEdges :: Map String BasicBlock -> Map String [String]
buildEdges blocks = Map.fromList $ map (\bb -> (bbLabel bb, getSuccessors bb)) (Map.elems blocks)
  where
    addrToBBLabel = Map.fromList [ (parseAddress (address (head (bbInstructions bb))), bbLabel bb) | bb <- Map.elems blocks ]

    getSuccessors bb =
      let lastInstr = last (bbInstructions bb)
          lastAddr  = parseAddress (address lastInstr)
          mnem      = mnemonic lastInstr
      in case mnem of
           "jmp" -> getJumpTargets lastInstr
           m | m `elem` ["je", "jne", "jg", "jge", "jl", "jle", "ja", "jb"] ->
                   getJumpTargets lastInstr ++ getFallThrough lastAddr
           "ret" -> []
           "call" -> getCallTarget lastInstr ++ getFallThrough lastAddr
           _      -> getFallThrough lastAddr

    getJumpTargets instr =
      case parseOperandAddress (operands instr) of
        Just targetAddr -> maybeToList $ Map.lookup targetAddr addrToBBLabel
        Nothing         -> []

    getCallTarget instr =
      case parseOperandAddress (operands instr) of
        Just targetAddr -> maybeToList $ Map.lookup targetAddr addrToBBLabel
        Nothing         -> []

    getFallThrough lastAddr =
      let nextAddr = nextInstructionAddress lastAddr
      in case nextAddr of
           Just addr -> maybeToList $ Map.lookup addr addrToBBLabel
           Nothing   -> []

    addrList = Map.keys addrToBBLabel
    sortedAddrList = sort addrList

    nextInstructionAddress addr =
      let nextAddrs = dropWhile (<= addr) sortedAddrList
      in case nextAddrs of
           (nextAddr:_) -> Just nextAddr
           []           -> Nothing
