module Main where

import Prelude hiding (LT, EQ, GT)

import ASM
import Runtime (allocateMemory, jit)

import Data.Word (Word32, Word16, Word8)
import Control.Concurrent (threadDelay)

dump :: (Integral a, Show a) => [a] -> IO ()
dump = mapM_ (Prelude.putStrLn . hex)

loopProgram :: ASM ()
loopProgram = do
  emit $ MOVI Mk0 1 X0
  emit $ MOVI Mk0 1 X11
  emit $ ADD NoCarry X0 X0 X11
  emit $ CMPI X0 41
  emit $ BCOND LE (-2)
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

  let jitm = assemble mem loopProgram
  case jitm of
    Left msg -> putStrLn $ "failed with:" ++ msg
    Right jitst -> do
      let machCode = mach jitst
      dump machCode

      fn <- jit mem machCode
      threadDelay 100000
      -- print "copy program from vec to mem"
      res <- fn
      -- print "run asm function"
      putStrLn $ "Result:" <> show res
