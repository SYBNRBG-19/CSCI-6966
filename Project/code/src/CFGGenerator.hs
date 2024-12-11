module CFGGenerator (generateCFG, CFG(..), BasicBlock(..)) where

import BinaryParser (Instruction(..))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe, maybeToList)
import Numeric (readHex, showHex)
import Data.List (sort, isPrefixOf)
import Data.Char (toLower)

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
    -- Pair each instruction with its parsed address
    instrAddrs = map (\instr -> (instr, parseAddress (address instr))) instructions

    -- Collect all basic block starting addresses
    bbStarts = collectBasicBlockStarts instrAddrs

    -- Build basic blocks from instructions
    blocksList = extractBasicBlocks instrAddrs bbStarts

    -- Create a map from block labels to BasicBlock
    blocks = Map.fromList [ (bbLabel bb, bb) | bb <- blocksList ]

    -- Build edges based on control flow
    edges = buildEdges blocks

-- Function to parse hexadecimal addresses
parseAddress :: String -> Integer
parseAddress s = case readHex s of
                  [(addr, "")] -> addr
                  _            -> error $ "Invalid address: " ++ s

-- Collect starting addresses of basic blocks
collectBasicBlockStarts :: [(Instruction, Integer)] -> Set Integer
collectBasicBlockStarts instrAddrs = Set.fromList $ startAddr : jumpTargets ++ postJumpAddrs
  where
    startAddr = snd (head instrAddrs)
    jumpTargets = mapMaybe getJumpTarget instrAddrs
    postJumpAddrs = mapMaybe getPostJumpAddress (zip instrAddrs (tail instrAddrs))

    -- Extract jump and call targets from instructions
    getJumpTarget (instr, _) =
      if mnemonic instr `elem` jumpMnemonics
        then parseOperandAddress (operands instr)
        else Nothing
      where
        jumpMnemonics = ["jmp", "je", "jne", "jg", "jge", "jl", "jle", "ja", "jb", "call"]

    -- Extract post-jump addresses (instructions immediately following any block-ending instruction)
    getPostJumpAddress ((instr, _), (_, nextAddr)) =
      if mnemonic instr `elem` blockEndMnemonics
        then Just nextAddr
        else Nothing
      where
        blockEndMnemonics = ["jmp", "je", "jne", "jg", "jge", "jl", "jle", "ja", "jb", "call", "ret"]

-- Build basic blocks from the instruction list
extractBasicBlocks :: [(Instruction, Integer)] -> Set Integer -> [BasicBlock]
extractBasicBlocks instrAddrs bbStarts = go instrAddrs Nothing []
  where
    -- Recursive helper function
    go :: [(Instruction, Integer)] -> Maybe (String, [Instruction]) -> [BasicBlock] -> [BasicBlock]
    go [] Nothing builtBlocks = builtBlocks
    go [] (Just (label, instrs)) builtBlocks = builtBlocks ++ [BasicBlock label (reverse instrs)]
    go ((i, addr):rest) currentBlock builtBlocks
      | Set.member addr bbStarts =
          -- Encountering a block start: close the current block if any, start a new one
          let builtBlocks' = case currentBlock of
                                Nothing -> builtBlocks
                                Just (lbl, instrs) -> builtBlocks ++ [BasicBlock lbl (reverse instrs)]
              newLabel = formatAddress addr
          in go rest (Just (newLabel, [i])) builtBlocks'
      | otherwise =
          case currentBlock of
            Nothing -> 
              -- If we have no current block, start a new one here even if not in bbStarts
              let label = formatAddress addr
              in go rest (Just (label, [i])) builtBlocks
            Just (lbl, instrs) ->
              let newInstrs = i : instrs
                  endsBlock = mnemonic i `elem` endMnemonics
                  builtBlocks' = if endsBlock
                                 then builtBlocks ++ [BasicBlock lbl (reverse newInstrs)]
                                 else builtBlocks
                  newCurrentBlock = if endsBlock
                                    then Nothing
                                    else Just (lbl, newInstrs)
              in go rest newCurrentBlock builtBlocks'
    endMnemonics = ["jmp", "je", "jne", "jg", "jge", "jl", "jle", "ja", "jb", "ret", "call"]

    -- Helper function to format address as hex string without "0x"
    formatAddress :: Integer -> String
    formatAddress addr = showHex addr ""

-- Build edges between basic blocks based on control flow
buildEdges :: Map String BasicBlock -> Map String [String]
buildEdges blocks = Map.fromList $ map (\bb -> (bbLabel bb, getSuccessors bb)) (Map.elems blocks)
  where
    -- Map from address to block label for quick lookup
    addrToBBLabel = Map.fromList 
      [ (parseAddress (address (head (bbInstructions bb))), bbLabel bb) 
      | bb <- Map.elems blocks, not (null (bbInstructions bb)) ]

    -- Sorted list of block starting addresses
    sortedAddrList = sort $ Map.keys addrToBBLabel

    -- Determine the successors of a given basic block
    getSuccessors :: BasicBlock -> [String]
    getSuccessors bb =
      if null (bbInstructions bb)
        then []
        else
          let lastInstr = last (bbInstructions bb)
              mnem      = mnemonic lastInstr
          in case mnem of
               "jmp" -> getJumpTargets lastInstr
               m | m `elem` ["je", "jne", "jg", "jge", "jl", "jle", "ja", "jb"] ->
                       let jumpTargets = getJumpTargets lastInstr
                           fallThrough = getFallThrough (parseAddress (address lastInstr))
                       in jumpTargets ++ fallThrough
               "ret" -> []
               "call" ->
                 let callTargets = getCallTarget lastInstr
                     fallThrough = getFallThrough (parseAddress (address lastInstr))
                 in callTargets ++ fallThrough
               _      -> getFallThrough (parseAddress (address lastInstr))

    -- Extract jump targets from an instruction
    getJumpTargets :: Instruction -> [String]
    getJumpTargets instr =
      case parseOperandAddress (operands instr) of
        Just targetAddr ->
          let targetLabel = Map.lookup targetAddr addrToBBLabel
          in maybeToList targetLabel
        Nothing -> []

    -- Extract call targets from an instruction
    getCallTarget :: Instruction -> [String]
    getCallTarget instr =
      case parseOperandAddress (operands instr) of
        Just targetAddr ->
          let targetLabel = Map.lookup targetAddr addrToBBLabel
          in maybeToList targetLabel
        Nothing -> []

    -- Determine the fall-through block (the next sequential block)
    getFallThrough :: Integer -> [String]
    getFallThrough lastAddr =
      let nextAddr = nextInstructionAddress lastAddr sortedAddrList
      in case nextAddr of
           Just addr -> let nextLabel = Map.lookup addr addrToBBLabel
                        in maybeToList nextLabel
           Nothing   -> []

    -- Function to find the address of the next instruction after a given address
    nextInstructionAddress :: Integer -> [Integer] -> Maybe Integer
    nextInstructionAddress addr sortedAddrs =
      case dropWhile (<= addr) sortedAddrs of
        (nextAddr:_) -> Just nextAddr
        []           -> Nothing

-- Function to parse operand addresses (e.g., "0x2004") into Integer
parseOperandAddress :: String -> Maybe Integer
parseOperandAddress s =
    let s' = trim s
    in if "0x" `isPrefixOf` s'
         then parseHex (drop 2 s')
         else parseHex s'
  where
    parseHex :: String -> Maybe Integer
    parseHex hexStr = case readHex (takeWhile isHexDigit hexStr) of
                       [(n, "")] -> Just n
                       _         -> Nothing

    -- Helper function to trim leading and trailing whitespace
    trim :: String -> String
    trim = f . f
       where f = reverse . dropWhile (`elem` [' ', '\t'])

-- Helper function to check if a character is a hexadecimal digit
isHexDigit :: Char -> Bool
isHexDigit c = c `elem` "0123456789abcdefABCDEF"
