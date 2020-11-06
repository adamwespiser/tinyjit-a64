### Readme

tinyjit-a64 is a project to map assembly code to exectuable binary, and use this capability in a very crude JIT for Haskell.    

#### Documentation
To find out more about ARM processors, aarch64 architecture, and the a64 instruction set, I've been using the following resources:

## Inspiration
[Monads to Machine Code](https://www.stephendiehl.com/posts/monads_machine_code.html). Another venerable Stephen Diehl Post, this one on a JIT encoding of X86.
[Handy by Ian Connolly](https://github.com/minuteman3/Handy) An ARM emulator written in Haskell

## Helpful Resources
[Index of Arm Intstructions](http://shell-storm.org/armv8-a/ISA_v85A_A64_xml_00bet8/xhtml/index.html) Enumerated list of all the ARM instructions with descriptions.    
[Arm Architecture Reference Manual](https://developer.arm.com/documentation/ddi0487/fc/) This is was you'll need if you are going to actually encode asm instructions into binary.    
[A64 Instruction Set Encoding](https://github.com/CAS-Atlantic/AArch64-Encoding/blob/master/binary%20encodding.pdf). Just the encodings.    
[Guide to AARM64 by modexp](https://modexp.wordpress.com/2018/10/30/arm64-assembly/#registers). Good intro to asm.    
[Arm book by toves](http://www.toves.org/books/arm/) Nice explaination of various ARM features.    

## Todo

#### C calls, basically printf

Figure out how to call Functions in C.    
This will be kind of tricky, since we have a limit on how far we can jump, and our C code is within    
a dynamically linked library, maybe linked within that range.    
The approach, if you cannot jump directly, is to jump as far as you can go in the right direciton    
and then do another jump to get you all the way there.   
This approach, implemented in ARM/ghc, is described here:  https://gitlab.haskell.org/ghc/ghc/-/merge_requests/3641/diffs#6997a0cd37829d3d08db1c48185aaa3d7dfc3e6b_0_1012   
Is there a way to call "printf" without doing this?    

#### Where we can go with this project

Have some IR definition:

```

data IR = IRBlock (NonEmpty Statement)

data Term =
    BinOp Var Term Term
  | UnOp Var Term

data Statement =
  | Label String
  | Jump String
  | Assign Variable Term
  | Void Term

data Expr =
  | If Term Statement Statement
  | DoWhile Term Statement
  | Return Term

data Lit = LitString Text | LitInt Int64

```

Where the focus is on control flow, within what would be a single function.    

Then, we do a very simple process to generate the assembly code:    

```
:: IR
  -> Instructions (no registers)
  -> Basic Blocks
  -> Live Ranges
  -> Linear Scan Register Assignment
  -> Code Generation
  -> Callable ASM
```

[Linear Scan](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf) (since this is a JIT after all :) ).    

