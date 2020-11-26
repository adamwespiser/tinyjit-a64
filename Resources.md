```
::   quasiquoted expression in Haskell
  -> IR + [referenced memory locations]
  -> Instructions (w/ "virtual" registers)
  -> Control Flow Graph (CFG) / Basic Blocks
  -> Meta Variable info (Live Ranges / du-chains)
  -> Linear Instructions in Bytecode
```

## A Tiny JIT for fast calculation
The goal of this project is to build a simple JIT, or just in time compiler that interpolate program values into a language compiled into fast assembly code.    
A critical objective of this project is to experiment with programming language ideas, and code for a tutorial.    

## Demonstration/use case
One simple use case would be taking an array, and taking the sum. That might look something like this:    
```
main = do
  x <- (newArray (0,9) :: MArray _ Int32)
  let jitCode = [jit|
    assign sum = 0;
    forloop(assign i = 0, eval i < 100, assign i++, assign sum = &{x} + i)
    return sum;
    |]
  location <- allocteCode jitCode
  case runJitCode location of
    Left e -> print "some error"
    Right success -> print $ "result of computation is: " <> success
```

A couple of things to note here:    

* The language for this jit is C-like, but should probably be much simpler, including basic assign statement, variables, expressions, and control structures like loops and maybe labels and gotos.  Nice-to-haves on the functional side would be arrays, and functions.    
* In terms of the compiler front-end, I'd like to keep this part of the project as simple as possible, at the expense of language safety.    
* `x` in our example is an array, but it needs to be accessible, its memory contigiuous, and not accessible to garbage collection or anything else.    

## Steps in the Algorithm

### Quasiquoter to IR
This part can probably be the last component to be implemented, and is the most depdendent, but specifying for other steps.    
[quasiquotation](https://wiki.haskell.org/Quasiquotation)

### Instruction Selection
` IR -> Instructions `    
This is an NP class problem. The 80/20 solution here would be macro expansion.    
[Survey On Instruction Selection](https://arxiv.org/pdf/1306.4898.pdf)    

### Basic Blocks
` Instructions -> CFG`    
Once you have instructions, as in assembly instructions, picked out, basic blocks are pretty trivial to set up. The big component of this step is to move everything into an effecient graph representation.    
[Wikipedia on Basic Blocks](https://en.wikipedia.org/wiki/Basic_block)    

#### Register Allocation
It's probably fine to just yolo it and just use a new register for every variabel (naive), but in case that doesn't work...    
[Brigg's Thesis](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.32.2766&rep=rep1&type=pdf)    
[Linear Scan](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf)     

## Data Structures
Starting with an IR, something similar to C, and for this project assume that things are correctly typed.    
For representing graphs, use the Haskell library, [Algebraic Graphs (white paper)](https://dl.acm.org/doi/pdf/10.1145/3122955.3122956)    



