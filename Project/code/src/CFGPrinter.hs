module CFGPrinter (printCFG) where

import BinaryParser (Instruction(..))
import CFGGenerator (CFG(..), BasicBlock(..))
import Data.Map (Map)
import qualified Data.Map as Map

printCFG :: CFG -> IO ()
printCFG (CFG blocks edges) = do
  putStrLn "Blocks:"
  mapM_ printBlock (Map.elems blocks)
  putStrLn "\nEdges:"
  mapM_ printEdge (Map.toList edges)
  putStrLn ""
  where
    printBlock :: BasicBlock -> IO ()
    printBlock (BasicBlock label instrs) = do
      putStrLn $ "Block Label: " ++ label
      putStrLn "Instructions:"
      mapM_ printInstr instrs
      putStrLn ""

    printInstr :: Instruction -> IO ()
    printInstr instr = do
      putStrLn $ "  Instruction {address = \"" ++ address instr ++ "\", bytes = \"" ++ bytes instr ++ "\", mnemonic = \"" ++ mnemonic instr ++ "\", operands = \"" ++ operands instr ++ "\"}"

    printEdge :: (String, [String]) -> IO ()
    printEdge (from, tos) = do
      putStrLn $ from ++ " -> " ++ show tos
