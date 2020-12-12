{-# LANGUAGE BangPatterns #-}

module Main where

import Prelude hiding (LT, EQ, GT)

import ASM
import Runtime (asciz, allocateMemory, extern, jit)

import Data.Word (Word32, Word16, Word8)
import Data.Bits
import Control.Concurrent (threadDelay)

dump :: (Integral a, Show a) => [a] -> IO ()
dump = mapM_ (Prelude.putStrLn . hex)


moveImm32 :: Reg -> Word32 -> ASM ()
moveImm32 reg word = do
  emit $ MOV ClearBits Mk0 reg lower
  emit $ MOV KeepBits Mk16 reg upper
  where
    x :: Int
    x = fromInteger . toInteger $ word
    mask :: Int
    mask  = fromInteger . toInteger $ createMask 16 `shiftL` 16
    upper :: Word16
    upper = fromInteger . toInteger $ ((x .&. mask) `shiftR` 16)
    lower :: Word16
    lower = fromInteger . toInteger $ clampShift x 16 0

testMovImm :: Int -> ASM ()
testMovImm int  = do
  let word = fromInteger . toInteger $ int
  moveImm32 X0 word
  emit $ RET Nothing

puts :: Word32 -> Word32 -> ASM ()
puts msg fnptr = do
  emit $ STP PPreIndex SP FP LR (-32)
  moveImm32 X0 msg
  moveImm32 X17 fnptr
  -- emit $ BR X17
  emit $ LDP PPostIndex SP FP LR 32
  -- emit $ MOV_reg X17 X0
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

return1Function01 :: [Word32]
return1Function01 = 
  [ 0x52800020 -- mov     w0, #0x1      
  , 0xd65f03c0 -- ret
  ]

return1Function :: [Word32]
return1Function = 
  [ 0xd10043ff -- sub     sp, sp, #0x10
  , 0x52800020 -- mov     w0, #0x1      
  , 0xb9000fe0 -- str     w0, [sp, #12]
  , 0xb9400fe0 -- ldr     w0, [sp, #12]
  , 0x910043ff -- add     sp, sp, #0x10
  , 0xd65f03c0 -- ret
  ]


main :: IO ()
main = do
  mem <- allocateMemory $ 32 * 1024
  fnptr <- extern "puts"
  print $ "Function Ptr Location " <> show fnptr
  msg <- asciz "Hello Low Level Worrld"
  print $ "Mesg. Memory Location " <> show msg
  print $ "Code. Memory Location " <> show mem

  -- let jitm = assemble mem loopProgram
  -- let jitm = assemble mem $ testMovImm 1431655765
  let jitm = assemble mem $ puts msg fnptr
  case jitm of
    Left msg -> putStrLn $ "failed with:" ++ msg
    Right jitst -> do
      let machCode = mach jitst
      dump machCode

      !fn <- jit mem machCode
      threadDelay 1000 -- XXX this is bad design
      !res <- fn
      -- print "copy program from vec to mem"
      -- !res <- fn
      -- print "run asm function"
      putStrLn $ "Result:" <> show res
