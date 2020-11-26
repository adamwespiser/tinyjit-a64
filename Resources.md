```
::   IR
  -> Instructions (no registers)
  -> CFGs / Basic Blocks
  -> Meta Variable info (Live Ranges / du-chains)
  -> Linear Instructions in Bytecode
```

## Steps in the Algorithm
### Instruction Selection
` IR -> Instructions `    
[Survey On Instruction Selection](https://arxiv.org/pdf/1306.4898.pdf)    

### Basic Blocks
` Instructions -> CFG`
[Wikipedia on Basic Blocks](https://en.wikipedia.org/wiki/Basic_block)    

#### Register Allocation
It's probably fine to just yolo it and just use a new register for every variabel (naive), but in case that doesn't work...    
[Brigg's Thesis](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.32.2766&rep=rep1&type=pdf)    
[Linear Scan](http://web.cs.ucla.edu/~palsberg/course/cs132/linearscan.pdf)     

## Data Structures
Starting with an IR, something similar to C, and for this project assume that things are correctly typed.    
For representing graphs, use the Haskell library, [Algebraic Graphs (white paper)](https://dl.acm.org/doi/pdf/10.1145/3122955.3122956)    



