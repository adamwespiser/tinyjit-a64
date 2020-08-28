module ASM where

import Data.Word

data Reg
 = X0 --arguments and retun values
 | X1
 | X2
 | X3
 | X4
 | X5
 | X6
 | X7
 | X8 -- temp values
 | X9
 | X10
 | X11
 | X12
 | X13
 | X14
 | X15
 | X16
 | X17
 | X18
 | X19 -- calle-saved registers
 | X20
 | X21
 | X22
 | X23
 | X24
 | X25
 | X26
 | X27
 | X28
 | X29  -- frame pointer
 | X30  -- stack pointer
 deriving (Eq, Show, Enum)


data Val
 = I Word8
 | R Reg
 deriving (Eq, Show)

data SubHeadingField
  = Variant32Bit
  | Variant64Bit
  deriving (Eq, Show, Enum)

data ConditionFlag
 = EQ -- 0000 -- equal
 | NE -- 0001 -- not equal
 | CS -- 0010 -- carry set
 | CC -- 0011 -- carry clear
 | PL -- 0101 -- plus, positive or zero
 | VS -- 0110 -- overflow
 | VC -- 0111 -- no overflow
 | HI -- 1000 -- unsigned higher
 | LS -- 1001 -- unsigned lower or same
 | GE -- 1010 -- signed great than or equal
 | LT -- 1011 -- signed less than
 | GT -- 1100 -- signed greater than
 | LE -- 1101 -- signed less than or equal
 | AL -- 1110 - -always
 | NV -- 1111 -- always

-- Table C4-1 Main encoding table of A64 Instruction set
data Op0
  = Reserverd -- 0000
  | Unallocated -- 0001
  | SVEInstr     -- 0010
  | DataProcImm -- 100x
  | Branch -- 101x
  | LoadStore -- x1x0
  | DataProcReg -- x101
  | DataProcScalar -- x111

type Offset = Word8 -- number of Bytes offset

data MovK = Mk0 | Mk16 | Mk32 | Mk48 deriving (Show, Eq, Enum)

data Instr
  = STR Reg Reg Offset
  | LDR Reg Reg Offset
  | MOV Reg Reg
  | MOVK MovK Word16 Reg
  | RET
  | SVC Word16



