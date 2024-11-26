module BufferOverflowCheckerSpec (spec) where

import Test.Hspec
import Types
import CFGGenerator
import BufferOverflowChecker
import BinaryParser (Instruction(..))

import qualified Data.Map as Map

spec :: Spec
spec = describe "BufferOverflowChecker" $ do
  it "detects buffer overflow vulnerability in calls to strcpy" $ do
    let cfg = createTestCFG [instrCall "strcpy"]
    let vulns = checkBufferOverflow cfg
    length vulns `shouldBe` 1
    vulnType (head vulns) `shouldBe` "Buffer Overflow Vulnerability"
    description (head vulns) `shouldContain` "strcpy"

  it "does not report vulnerability when safe functions are called" $ do
    let cfg = createTestCFG [instrCall "safeFunction"]
    let vulns = checkBufferOverflow cfg
    vulns `shouldBe` []

  it "detects buffer overflow vulnerability in calls to gets" $ do
    let cfg = createTestCFG [instrCall "gets"]
    let vulns = checkBufferOverflow cfg
    length vulns `shouldBe` 1
    vulnType (head vulns) `shouldBe` "Buffer Overflow Vulnerability"
    description (head vulns) `shouldContain` "gets"

-- Helper functions
createTestCFG :: [Instruction] -> CFG
createTestCFG instrs = CFG
  { cfgBlocks = Map.fromList [("0x1000", BasicBlock "0x1000" instrs)]
  , cfgEdges = Map.empty
  }

instrCall :: String -> Instruction
instrCall funcName = Instruction
  { address = "0x1000"
  , bytes = ""
  , mnemonic = "call"
  , operands = funcName
  }
