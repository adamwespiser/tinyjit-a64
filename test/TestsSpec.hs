-- | Description: tests jit encoder
module TestsSpec (spec) where

import Runtime
import ASM

import Test.Hspec
import Data.Word (Word32)
import Data.Text

import Control.Concurrent (threadDelay)


spec :: Spec
spec = do
  describe "Can Execute Memory" memoryExecuteTests
  describe "Can Encode ASM Instructions" asmExecuteTests


hexReturnOneSimple :: [Word32]
hexReturnOneSimple = 
  [ 0x52800020 -- mov     w0, #0x1      
  , 0xd65f03c0 -- ret
  ]

hexReturnOneComplex :: [Word32]
hexReturnOneComplex = 
  [ 0xd10043ff -- sub     sp, sp, #0x10
  , 0x52800020 -- mov     w0, #0x1      
  , 0xb9000fe0 -- str     w0, [sp, #12]
  , 0xb9400fe0 -- ldr     w0, [sp, #12]
  , 0x910043ff -- add     sp, sp, #0x10
  , 0xd65f03c0 -- ret
  ]


runHex :: [Word32] -> IO (Either Text Int)
runHex hex = do
  mem <- allocateMemory $ 32 * 1024
  !fn <- jit mem hex
  threadDelay 1000
  !res <- fn
  pure . Right $ res

runASM :: ASM a -> IO (Either Text Int)
runASM asm = do
  mem <- allocateMemory $ 32 * 1024
  case assemble mem asm of
    Left msg -> pure . Left $ "failed to discharge ASM into hex string"
    Right jitst -> do
      let machCode = mach jitst
      !fn <- jit mem machCode
      threadDelay 1000
      !res <- fn
      pure . Right $ res

memoryExecuteTests :: SpecWith ()
memoryExecuteTests = do
  describe "can execute hex asm" $ do
    it "simple asm return" $ do
      (runHex hexReturnOneSimple) `shouldReturn` (Right $ (1 :: Int))
    it "complex asm return" $ do
      (runHex hexReturnOneComplex) `shouldReturn` (Right $ (1 :: Int))


asmReturnSimple :: ASM ()
asmReturnSimple = do
  emit $ MOV ClearBits Mk0 X0 17
  emit $ RET Nothing

loopProgram :: ASM ()
loopProgram = do
  emit $ STP PPreIndex SP FP LR (-32)
  emit $ MOV ClearBits Mk0 X0 1
  emit $ MOV ClearBits Mk0 X11 1
  emit $ ADD NoCarry X0 X0 X11
  emit $ CMPI X0 41
  emit $ BCOND LE (-2)
  emit $ LDP PPostIndex SP FP LR 32
  emit $ RET Nothing

testMovImm :: Int -> ASM ()
testMovImm int  = do
  let word = fromInteger . toInteger $ int
  moveImm32 X0 word
  emit $ RET Nothing

asmExecuteTests :: SpecWith ()
asmExecuteTests = do
  describe "can encode and execute ASM instructions" $ do
    it "simple return" $ do
      (runASM asmReturnSimple) `shouldReturn` (Right (17 :: Int))
    it "loop program" $ do
      (runASM loopProgram) `shouldReturn` (Right (42 :: Int))
    it "can inject integer into code" $ do
      let x = 413555 :: Int
      (runASM $ testMovImm x) `shouldReturn` (Right x)
     


