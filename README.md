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
