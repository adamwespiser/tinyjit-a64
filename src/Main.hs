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

-- Doesn't work, segfault, not sure why???
puts :: Word32 -> Word32 -> ASM ()
puts msg fnptr = do
  emit $ STP PPreIndex SP FP LR (-32)
  moveImm32 X0 msg
  moveImm32 X17 fnptr
  -- emit $ BR X17
  emit $ LDP PPostIndex SP FP LR 32
  -- emit $ MOV_reg X17 X0
  emit $ RET Nothing


main :: IO ()
main = do
  mem <- allocateMemory $ 32 * 1024
  fnptr <- extern "puts"
  print $ "Function Ptr Location " <> show fnptr
  msg <- asciz "Hello Low Level Worrld"
  print $ "Mesg. Memory Location " <> show msg
  print $ "Code. Memory Location " <> show mem

  let jitm = assemble mem $ puts msg fnptr
  case jitm of
    Left msg -> putStrLn $ "failed with:" ++ msg
    Right jitst -> do
      let machCode = mach jitst
      dump machCode

      !fn <- jit mem machCode
      threadDelay 1000 -- XXX this is bad design
      !res <- fn
      putStrLn $ "Result:" <> show res
