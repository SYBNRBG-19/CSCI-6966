module FormatStringCheckerSpec (spec) where

import Test.Hspec
import CFGGenerator
import Types
import FormatStringChecker
import BinaryParser (Instruction(..), parseBinary)

import qualified Data.Map as Map

spec :: Spec
spec = describe "FormatStringChecker" $ do

  it "does not report vulnerability when safe functions are called" $ do
    let cfg = createTestCFG [instrCall "safeFunction"]
    let vulns = checkFormatStringVulnerabilities cfg
    vulns `shouldBe` []

  it "reports vulnerability when printf is called" $ do
    let cfg = createTestCFG [instrCall "401010"]  -- "printf@plt"
    let vulns = checkFormatStringVulnerabilities cfg
    vulns `shouldBe` [ Vulnerability
                        { vulnType = "Format String Vulnerability"
                        , description = "Possible format string vulnerability in call to printf"
                        , location = "0x1000"
                        } ]

  it "handles an empty CFG" $ do
    let cfg = CFG
                { cfgBlocks = Map.empty
                , cfgEdges = Map.empty
                }
    let vulns = checkFormatStringVulnerabilities cfg
    vulns `shouldBe` []
  
  it "parses a binary file into instructions" $ do
    instructions <- parseBinary "test-binaries/format_string.bin"
    let cfg = generateCFG instructions
    let vulns = checkFormatStringVulnerabilities cfg
    vulns `shouldBe` [ Vulnerability
                        { vulnType = "Format String Vulnerability"
                        , description = "Possible format string vulnerability in call to printf"
                        , location = "40104f"
                        } ]

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
