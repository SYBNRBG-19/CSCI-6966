module FormatStringCheckerSpec (spec) where

import Test.Hspec
import CFGGenerator
import Types
import FormatStringChecker
import BinaryParser (Instruction(..))

import qualified Data.Map as Map

spec :: Spec
spec = describe "FormatStringChecker" $ do
  it "detects format string vulnerability in calls to printf with user input" $ do
    let cfg = createTestCFG [instrCall "printf"]
    let vulns = checkFormatStringVulnerabilities cfg
    length vulns `shouldBe` 1
    vulnType (head vulns) `shouldBe` "Format String Vulnerability"
    description (head vulns) `shouldContain` "printf"

  it "does not report vulnerability when safe functions are called" $ do
    let cfg = createTestCFG [instrCall "safeFunction"]
    let vulns = checkFormatStringVulnerabilities cfg
    vulns `shouldBe` []

  it "detects format string vulnerability in calls to fprintf" $ do
    let cfg = createTestCFG [instrCall "fprintf"]
    let vulns = checkFormatStringVulnerabilities cfg
    length vulns `shouldBe` 1
    vulnType (head vulns) `shouldBe` "Format String Vulnerability"
    description (head vulns) `shouldContain` "fprintf"

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
