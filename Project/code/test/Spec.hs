import Test.Hspec

import qualified BinaryParserSpec
import qualified CFGGeneratorSpec
import qualified FormatStringCheckerSpec
import qualified BufferOverflowCheckerSpec
import qualified StackIntegrityCheckerSpec
import qualified VulnerabilityScannerSpec

main :: IO ()
main = hspec $ do
  describe "Binary Parsing Tests" BinaryParserSpec.spec
  describe "CFG Generator Tests" CFGGeneratorSpec.spec
  describe "Format String Checker Tests" FormatStringCheckerSpec.spec
  describe "Buffer Overflow Checker  Tests" BufferOverflowCheckerSpec.spec
  describe "Stack Integrity Checker Tests" StackIntegrityCheckerSpec.spec
  describe "Vulnerability Scanner Tests" VulnerabilityScannerSpec.spec
