module CFGGeneratorSpec where

import Test.Hspec
import Data.Map as Map
import CFGGenerator
import BinaryParser (Instruction(..))

spec :: Spec
spec = do
  describe "CFGGenerator" $ do
    it "generates a CFG with a single basic block for linear code" $ do
      let instructions = [ Instruction "1000" "55"      "push" "ebp"
                        , Instruction "1001" "89 e5"   "mov"  "ebp, esp"
                        , Instruction "1003" "5d"      "pop"  "ebp"
                        , Instruction "1004" "c3"      "ret"  ""
                        ]
      let cfg = generateCFG instructions
      -- Check that there is one basic block
      Map.size (cfgBlocks cfg) `shouldBe` 1
      -- Check that there are no edges
      let totalEdges = sum $ Prelude.map length $ Map.elems (cfgEdges cfg)
      totalEdges `shouldBe` 0

    it "generates correct CFG for code with an unconditional jump" $ do
      let instructions = [ Instruction "2000" "eb 02"   "jmp"  "0x2004"
                        , Instruction "2002" "90"      "nop"  ""
                        , Instruction "2004" "c3"      "ret"  ""
                        ]
      let cfg = generateCFG instructions
      -- Check that there are two basic blocks
      Map.size (cfgBlocks cfg) `shouldBe` 3
      -- Check edges
      let edges = cfgEdges cfg
      Map.member "2000" edges `shouldBe` True
      edges Map.! "2000" `shouldBe` ["2004"]

    it "generates correct CFG for code with a conditional jump" $ do
      let instructions = [ Instruction "3000" "74 04"   "je"   "0x3006"
                        , Instruction "3002" "90"      "nop"  ""
                        , Instruction "3003" "eb 02"   "jmp"  "0x3007"
                        , Instruction "3006" "90"      "nop"  ""
                        , Instruction "3007" "c3"      "ret"  ""
                        ]
      let cfg = generateCFG instructions
      -- Check that there are three basic blocks (since "3003" is not a separate basic block)
      Map.size (cfgBlocks cfg) `shouldBe` 4
      -- Check edges
      let edges = cfgEdges cfg
      edges Map.! "3000" `shouldMatchList` ["3006", "3002"]
      edges Map.! "3002" `shouldBe` ["3007"]
      edges Map.! "3006" `shouldBe` ["3007"]

    it "handles call instructions correctly in the CFG" $ do
      let instructions = [ Instruction "4000" "e8 05 00 00 00" "call" "0x400a"
                        , Instruction "4005" "90"             "nop"  ""
                        , Instruction "4006" "c3"             "ret"  ""
                        , Instruction "400a" "90"             "nop"  ""
                        , Instruction "400b" "c3"             "ret"  ""
                        ]
      let cfg = generateCFG instructions
      -- Check that there are three basic blocks
      Map.size (cfgBlocks cfg) `shouldBe` 3
      -- Check edges
      let edges = cfgEdges cfg
      edges Map.! "4000" `shouldMatchList` ["4005", "400a"]
      edges Map.! "4005" `shouldBe` []
      edges Map.! "400a" `shouldBe` []

    it "parses complex jump targets correctly" $ do
      let instructions = [ Instruction "5000" "0f 84 05 00 00 00" "je" "0x500b"
                        , Instruction "5006" "90"                "nop"  ""
                        , Instruction "5007" "eb 02"             "jmp"  "0x500d"
                        , Instruction "500b" "90"                "nop"  ""
                        , Instruction "500d" "c3"                "ret"  ""
                        ]
      let cfg = generateCFG instructions
      -- Check that there are four basic blocks
      Map.size (cfgBlocks cfg) `shouldBe` 4
      -- Check edges
      let edges = cfgEdges cfg
      edges Map.! "5000" `shouldMatchList` ["500b", "5006"]
      edges Map.! "5006" `shouldBe` ["500d"]
      edges Map.! "500b" `shouldBe` ["500d"]

    it "correctly identifies basic block starts with fall-through" $ do
      let instructions = [ Instruction "6000" "74 04"   "je"   "0x6006"
                        , Instruction "6002" "90"      "nop"  ""
                        , Instruction "6003" "90"      "nop"  ""
                        , Instruction "6004" "90"      "nop"  ""
                        , Instruction "6006" "c3"      "ret"  ""
                        ]
      let cfg = generateCFG instructions
      -- Check that there are three basic blocks
      Map.size (cfgBlocks cfg) `shouldBe` 3
      -- Check edges
      let edges = cfgEdges cfg
      edges Map.! "6000" `shouldMatchList` ["6006", "6002"]
      edges Map.! "6002" `shouldBe` ["6006"]
      edges Map.! "6006" `shouldBe` []

    it "handles loops correctly in the CFG" $ do
      let instructions = [ Instruction "7000" "83 c0 01" "add"  "eax, 1"
                        , Instruction "7003" "83 f8 05" "cmp"  "eax, 5"
                        , Instruction "7006" "7c f8"    "jl"   "0x7000"
                        , Instruction "7008" "c3"       "ret"  ""
                        ]
      let cfg = generateCFG instructions
      -- Check that there are two basic blocks
      Map.size (cfgBlocks cfg) `shouldBe` 2
      -- Check edges
      let edges = cfgEdges cfg
      edges Map.! "7000" `shouldMatchList` ["7000", "7008"]
      edges Map.! "7008" `shouldBe` []
