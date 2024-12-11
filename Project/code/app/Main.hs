module Main where

import System.Environment (getArgs)
import BinaryParser (parseBinary, Instruction(..))
import CFGGenerator (generateCFG)
import VulnerabilityScanner (scanForVulnerabilities)
import ReportGenerator (generateReport)
import CFGPrinter (printCFG)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [binaryPath] -> do
      instructions <- parseBinary binaryPath
      mapM_ printInstruction instructions
      let cfg = generateCFG instructions
      printCFG cfg
      let vulnerabilities = scanForVulnerabilities cfg
      generateReport vulnerabilities
      putStrLn ""
   --  _ -> putStrLn "Usage: x86-vulnerability-checker <binary-file>"

printInstruction :: Instruction -> IO ()
printInstruction instr = do
  putStrLn $ "Address:   " ++ address instr
  putStrLn $ "Bytes:     " ++ bytes instr
  putStrLn $ "Mnemonic:  " ++ mnemonic instr
  putStrLn $ "Operands:  " ++ operands instr
  putStrLn ""  -- Blank line to separate instructions
