module BinaryParserSpec where

import Test.Hspec
import BinaryParser

spec :: Spec
spec = do
  describe "parseBinaryBase" $ do
    it "parses a binary file into instructions" $ do
      instructions <- parseBinary "test-binaries/simple.bin"
      length instructions `shouldSatisfy` (> 0)
    
    it "handles invalid binary input gracefully" $ do
      let input = "invalidBinary"
      parseBinary input `shouldThrow` anyException

  describe "parseObjdumpOutput" $ do
    it "parses a valid objdump output line into an Instruction" $ do
      let line = "400080: 48 89 e5        mov    %rsp,%rbp"
      let expected = Instruction "400080" "48 89 e5" "mov" "%rsp,%rbp"
      parseObjdumpOutput line `shouldBe` [expected]

    it "returns an empty list for a line that doesn't represent an instruction" $ do
      let line = "This is not an instruction line"
      parseObjdumpOutput line `shouldBe` []

    it "parses multiple lines of objdump output" $ do
      let output = unlines
            [ "400080: 48 89 e5           mov    %rsp,%rbp"
            , "400083: 89 7d fc           mov    %edi,-0x4(%rbp)"
            , "400086: 48 83 ec 10        sub    $0x10,%rsp"
            ]
      let expected = 
            [ Instruction "400080" "48 89 e5" "mov" "%rsp,%rbp"
            , Instruction "400083" "89 7d fc" "mov" "%edi,-0x4(%rbp)"
            , Instruction "400086" "48 83 ec 10" "sub" "$0x10,%rsp"
            ]
      parseObjdumpOutput output `shouldBe` expected

    it "parses instructions from an objdump file" $ do
      -- Simulating parseBinary as if it reads output from objdump
      let mockOutput = unlines
            [ "400080: 48 89 e5            mov    %rsp,%rbp"
            , "400083: 89 7d fc            mov    %edi,-0x4(%rbp)"
            ]
      let expected = 
            [ Instruction "400080" "48 89 e5" "mov" "%rsp,%rbp"
            , Instruction "400083" "89 7d fc" "mov" "%edi,-0x4(%rbp)"
            ]
      -- Mocking readProcess for the purpose of testing
      let mockReadProcess _ _ _ = return mockOutput
      result <- mockReadProcess "objdump" ["-d", "dummyPath"] ""
      parseObjdumpOutput result `shouldBe` expected

  describe "parseBinaryAdvance" $ do
    let expected = 
            [ Instruction "1000" "48 c7 c0 01 00 00 00" "mov" "$0x1,%rax"
            , Instruction "1007" "48 c7 c7 01 00 00 00" "mov" "$0x1,%rdi"
            , Instruction "100e" "48 8d 35 eb 1f 00 00" "lea" "0x1feb(%rip),%rsi"
            , Instruction "1015" "48 c7 c2 0e 00 00 00" "mov" "$0xe,%rdx"
            , Instruction "101c" "0f 05" "syscall" ""
            , Instruction "101e" "48 c7 c0 3c 00 00 00" "mov" "$0x3c,%rax"
            , Instruction "1025" "48 31 ff" "xor" "%rdi,%rdi"
            , Instruction "1028" "0f 05" "syscall" ""
            ]
    it "parses a binary file into instructions" $ do
      result <- parseBinary "test-binaries/simple.bin"
      result `shouldBe` expected
