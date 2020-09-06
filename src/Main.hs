module Main where

import ASM
import Runtime

main :: IO ()
main = do
  mem <- allocateMemory $ 256 * 1024
  jit mem [encode NOP] -- No-op, lets just see if the C stuff works first...
  res <- fn
  putStrLn $ "Result:" <> show res

