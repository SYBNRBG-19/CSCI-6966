{-# LANGUAGE OverloadedStrings #-}

module CFGPrinter (printCFG, writeCFGToFile) where

import System.IO (withFile, IOMode(..), hPutStrLn)
import CFGGenerator (CFG(..), BasicBlock(..))
import Data.Map (Map)
import qualified Data.Map as Map

-- | Print the CFG to standard output
printCFG :: CFG -> IO ()
printCFG cfg = do
    putStrLn "===== CONTROL FLOW GRAPH ====="
    putStrLn "Blocks:"
    Map.foldrWithKey printBlock (return ()) (cfgBlocks cfg)
    putStrLn "\nEdges:"
    Map.foldrWithKey printEdges (return ()) (cfgEdges cfg)
    putStrLn "===== END OF CFG ====="

  where
    printBlock :: String -> BasicBlock -> IO () -> IO ()
    printBlock label bb acc = do
      putStrLn $ "Block Label: " ++ label
      putStrLn "Instructions:"
      mapM_ (printInstr) (bbInstructions bb)
      putStrLn ""
      acc

    printInstr :: Show i => i -> IO ()
    printInstr instr = putStrLn $ "  " ++ show instr

    printEdges :: String -> [String] -> IO () -> IO ()
    printEdges from to acc = do
      putStrLn $ from ++ " -> " ++ show to
      acc

-- | Write the CFG to a specified file
writeCFGToFile :: FilePath -> CFG -> IO ()
writeCFGToFile filePath cfg = withFile filePath WriteMode $ \h -> do
    hPutStrLn h "===== CONTROL FLOW GRAPH ====="
    hPutStrLn h "Blocks:"
    Map.foldrWithKey (printBlock h) (return ()) (cfgBlocks cfg)
    hPutStrLn h "\nEdges:"
    Map.foldrWithKey (printEdges h) (return ()) (cfgEdges cfg)
    hPutStrLn h "===== END OF CFG ====="

  where
    printBlock h label bb acc = do
      hPutStrLn h $ "Block Label: " ++ label
      hPutStrLn h "Instructions:"
      mapM_ (printInstr h) (bbInstructions bb)
      hPutStrLn h ""
      acc

    printInstr h instr = hPutStrLn h $ "  " ++ show instr

    printEdges h from to acc = do
      hPutStrLn h $ from ++ " -> " ++ show to
      acc
