module StackIntegrityCheckerSpec (spec) where

import Test.Hspec
import CFGGenerator
import StackIntegrityChecker
import qualified Types as T
import BinaryParser (Instruction(..))

import qualified Data.Map as Map

spec :: Spec
spec = describe "StackIntegrityCheck" $ do
  it "detects stack imbalance due to missing pop instruction" $ do
    let instrs = [instrPush, instrPush, instrPop]
    let cfg = createTestCFG instrs
    let vulns = checkStackIntegrity cfg
    length vulns `shouldBe` 1
    T.vulnType (head vulns) `shouldBe` "Stack Integrity Vulnerability"
    T.description (head vulns) `shouldContain` "imbalance"

  it "does not report vulnerability when stack is balanced" $ do
    let instrs = [instrPush, instrPop]
    let cfg = createTestCFG instrs
    let imbalance = analyzeStackImbalance instrs  -- Calculate imbalance
    putStrLn ("Calculated stack imbalance for balanced case: " ++ show imbalance)  -- Debug output
    let vulns = checkStackIntegrity cfg
    vulns `shouldBe` []

  it "detects stack imbalance due to extra pop instruction" $ do
    let instrs = [instrPush, instrPop, instrPop]
    let cfg = createTestCFG instrs
    let vulns = checkStackIntegrity cfg
    length vulns `shouldBe` 1
    T.description (head vulns) `shouldContain` "imbalance"

-- Helper functions
createTestCFG :: [Instruction] -> CFG
createTestCFG instrs = CFG
  { cfgBlocks = Map.fromList [("0x1000", BasicBlock "0x1000" instrs)]
  , cfgEdges = Map.empty
  }

instrPush :: Instruction
instrPush = Instruction
  { address = "0x1000"
  , bytes = ""
  , mnemonic = "push"
  , operands = "eax"
  }

instrPop :: Instruction
instrPop = Instruction
  { address = "0x1001"
  , bytes = ""
  , mnemonic = "pop"
  , operands = "eax"
  }
