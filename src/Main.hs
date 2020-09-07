module Main where

import ASM
import Runtime

import Data.Word

dump :: [Word32] -> IO ()
dump = mapM_ (Prelude.putStrLn . hex)

asmProg :: [Word32]
-- asmProg = concat $ fmap encode $ [MOVK Mk0 (1::Word16) X0, RET Nothing]
asmProg = fmap encode [RET Nothing]

main :: IO ()
main = do
  dump asmProg
  mem <- allocateMemory $ 32 * 1024
  -- fn <- jit mem [encode $ RET Nothing] -- No-op, lets just see if the C stuff works first...
  fn <- jit mem asmProg
  res <- fn
  putStrLn $ "Result:" <> show res

