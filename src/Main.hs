module Main where


import Prelude hiding (LT, EQ, GT)

import ASM
import Runtime (allocateMemory, jit)

import Data.Word (Word32, Word16)
import Control.Concurrent (threadDelay)

dump :: [Word32] -> IO ()
dump = mapM_ (Prelude.putStrLn . hex)

asmProg :: [Word32]
asmProg = fmap encode prog1
-- asmProg = fmap encode $ [MOVI Mk0 (17::Word16) X0, RET Nothing]
-- asmProg = fmap encode [RET Nothing]
-- asmProg = return1Function01

prog1 = 
  [ MOVI Mk0 (10::Int) X12
  , MOVI Mk0 (1::Int) X13
  , MOVI Mk0 (1::Int) X14
  , NOP
  , ADD NoCarry X0 X13 X14
  , CMPI X0 10
  , BCOND LT 2 --branch if (cmp reg imm) reg is LT imm
  , B (-4)
  , RET Nothing
  ]

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
  dump asmProg
  mem <- allocateMemory $ 32 * 1024
  -- print "allocateMemory works"
  -- fn <- jit mem [encode $ RET Nothing] -- No-op, lets just see if the C stuff works first...
  fn <- jit mem asmProg
  threadDelay 100000
  -- print "copy program from vec to mem"
  res <- fn
  -- print "run asm function"
  putStrLn $ "Result:" <> show res
