module Main where

import System.Environment (getArgs)
import BinaryParser (parseBinary)
import CFGGenerator (generateCFG)
import VulnerabilityScanner (scanForVulnerabilities)
import ReportGenerator (generateReport)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [binaryPath] -> do
      instructions <- parseBinary binaryPath
      let cfg = generateCFG instructions
      let vulnerabilities = scanForVulnerabilities cfg
      generateReport vulnerabilities
    _ -> putStrLn "Usage: x86-vulnerability-checker <binary-file>"
