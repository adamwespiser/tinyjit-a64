{-# Language BinaryLiterals #-}

{-| Description: ASM representation of A64 ARM assembly

A64 is a fixed width encoding, but the location of various operands varies by
the type of the instruction.

A "good representation" for this problem stradles between the following:
 1. representing the computation of the higher level language
 2. contains enough specific details to actually be computed on a ARM processor

1. is pretty much the ASM type, and 2 is the encode function

-}
module ASM where
import Data.Word
import Data.Bits
import Data.Maybe

import Numeric (showHex)

data Reg --arguments and retun values
 = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
 -- temp values
 | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17 | X18
 -- calle-saved registers
 | X19  | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28
 -- frame pointer
 | FP
 -- stack pointer
 | SP
 deriving (Eq, Show, Enum)

-- Not sure if this is a good idea, in ARM Imm and Reg values are
-- not exchangable like they are in x86
data Val = I Word8 | R Reg deriving (Eq, Show)

-- | Is the Instr for 64 or 32 bit mode?
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
  | MOVK MovK Word16 Reg --move/shift
  | RET (Maybe Reg)
  | SVC Word16 --supervisor/system call
  | SYS --system call
  | Br -- branch
  | NOP
  | ADD Reg Reg

zero32 :: Word32
zero32 = fromInteger 0

-- | Lets just try something simple to get started
--   then once we get an idea for the pattern, we can use one
--   of the few known patterns here.
--   Note: Instruction size is 32bit for 32/64bit reg size
encode :: Instr -> Word32
encode (RET reg) = ((0b1101011001011111 :: Word32) `shiftL` 16)
  .|. ((fromIntegral @Int @Word32 . fromEnum $ fromMaybe X0 reg ) `shiftL` 5)
encode (MOVK movk imm16 reg) = zero32
  .|. (0b1 `shiftL` 31) -- 64 bit mode
  .|. (0b11100101 `shiftL` 23)  -- opc + data magic bits
  .|. ((fromIntegral @Int @Word32 . fromEnum $ movk) `shiftL` 21)
  .|. (fromIntegral $ imm16 `shiftL` 5)
  .|. (fromIntegral @Int @Word32 . fromEnum $ reg)
encode (NOP) = 0xD503201F
encode _ = zero32

-- | Instructions are in little-endian
toByteArray :: Word32 -> [Word8]
--toByteArray instr = fmap (fromInteger . toInteger . shiftFn) [24,16,8,0]
toByteArray instr = fmap (fromInteger . toInteger . shiftFn) [0,8,16,24] -- [24,16,8,0]
  where
   shiftFn shift = ((0xff `shiftL` shift) .&. instr) `shiftR` shift

hex :: (Integral a, Show a) => a -> String
hex x = showHex x ""

