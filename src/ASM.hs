{-# LANGUAGE BinaryLiterals,NumericUnderscores #-}

-- | Description: ASM representation of A64 ARM assembly
--
{- ARM_a64_instruction_set_architecture.pdf -}
-- A64 is a fixed width encoding, but the location of various operands varies by
-- the type of the instruction.
--
-- A "good representation" for this problem stradles between the following:
-- 1. representing the computation of the higher level language
-- 2. contains enough specific details to actually be computed on a ARM processor
--
-- 1. is pretty much the ASM type, and 2 is the encode function
module ASM where

import Runtime

import Data.Bits
import Data.Maybe (fromMaybe)
import Data.Word (Word16, Word32, Word8)
import Numeric (showHex)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

import Foreign.Ptr


import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

data Reg --arguments and retun values
 = X0 | X1 | X2 | X3 | X4 | X5 | X6 | X7
 -- temp values
 | X8 | X9 | X10 | X11 | X12 | X13 | X14 | X15 | X16 | X17 | X18
 -- calle-saved registers
 | X19  | X20 | X21 | X22 | X23 | X24 | X25 | X26 | X27 | X28
 -- frame pointer (x29)
 | FP
 -- procedure link register (x30)
 | LR
 -- stack pointer (x31)
 | SP
 deriving (Eq, Show, Enum)

-- zr is the Zero Register, which shares the same encoding as the stack pointer (x31)
zr :: Reg
zr = SP

-- Not sure if this is a good idea, in ARM Imm and Reg values are
-- not exchangable like they are in x86
data Val = I Word8 | R Reg deriving (Eq, Show)

-- | Is the Instr for 64 or 32 bit mode?
data SubHeadingField
  = Variant32Bit
  | Variant64Bit
  deriving (Eq, Show, Enum)

data CarryFlag
  = NoCarry
  | Carry
  deriving (Eq, Show, Enum)

-- | Offset is used in load/store for incrementing register memory addresses
data Offset
  = NoOffset     -- [r1, #16]
  | PostIndex    -- [r1], #16
  | PreIndex     -- [r1, #35]!
  deriving (Eq, Show)

instance Enum Offset where
    fromEnum = fromJust . flip lookup postIndexTable
    toEnum = fromJust . flip lookup (map swap postIndexTable)
postIndexTable = [(NoOffset, 0), (PostIndex, 1), (PreIndex, 4)]

data PairOffset
  = PPostIndex    -- [r1], #16
  | PSignedOffset -- [r1, #-16]
  | PPreIndex     -- [r1, #35]!
  deriving (Eq, Show)

instance Enum PairOffset where
    fromEnum = fromJust . flip lookup pairPostIndexTable
    toEnum = fromJust . flip lookup (map swap pairPostIndexTable)
pairPostIndexTable = [(PPostIndex, 1), (PSignedOffset, 2), (PPreIndex, 3)]


data ConditionFlag
  = EQ -- 0000 -- equal
  | NE -- 0001 -- not equal
  | CS -- 0010 -- carry set
  | CC -- 0011 -- carry clear
  | MI -- 0100 -- minus, negative
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
  deriving (Eq, Show, Enum)

-- Table C4-1 Main encoding table of A64 Instruction set
data Op0
  = Reserverd -- 0000
  | Unallocated -- 0001
  | SVEInstr -- 0010
  | DataProcImm -- 100x
  | Branch -- 101x
  | LoadStore -- x1x0
  | DataProcReg -- x101
  | DataProcScalar -- x111

data KeepBits = ClearBits | KeepBits deriving (Show, Eq, Enum)
data MovK = Mk0 | Mk16 | Mk32 | Mk48 deriving (Show, Eq, Enum)

data Instr
  = STR Offset Reg Reg Int -- offset=signed not supported
  | STP PairOffset Reg Reg Reg Int 
  | LDR Offset Reg Reg Int -- Offset Signed Not Supported
  | LDP PairOffset Reg Reg Reg Int 
  | MOV KeepBits MovK Reg Word16 --move/shift (KeepBits, True=MovK
  | MOV_reg Reg Reg
  | RET (Maybe Reg)
  | SVC Word16 --supervisor/system call
  | B Int -- branch, relative offset, Word32 ~ (+/-) * 4 * Instructions to jump
  | BL Int
  | BLR Reg
  | BR Reg
  | BCOND ConditionFlag Int
  | NOP
  | ADD CarryFlag Reg Reg Reg
  | SUB CarryFlag Reg Reg Reg
  | CMPI Reg Int
  deriving(Eq, Show)

zero32 :: Word32
zero32 = fromInteger 0

bitMode :: Integral a => a
bitMode = 0b1 {-0b0 for 32, 0b1 for 64-}

-- | Lets just try something simple to get started
--   then once we get an idea for the pattern, we can use one
--   of the few known patterns here.
--   Note: Instruction size is 32bit for 32/64bit reg size
encode :: Instr -> Word32
encode (STP index regN regT regT2 imm7) = zero32
  .|. (bitMode `shiftL` 31) -- 32 bit mode
  .|. ((0b00101 :: Word32) `shiftL` 27)
  .|. (codeTag index `shiftL` 23)
  .|. (clampShift imm7 7 15)
  .|. (codeTag regT2 `shiftL` 10)
  .|. (codeTag regN `shiftL` 5)
  .|. (codeTag regT)
{- LDP = STP & set bit 21 -}
encode (LDP index regN regT regT2 imm7) = zero32
  .|. (bitMode `shiftL` 31) -- 32 bit mode
  .|. ((0b00101 :: Word32) `shiftL` 27)
  .|. (codeTag index `shiftL` 23)
  .|. (0b1 `shiftL` 22) -- set the 'L' bit
  .|. (clampShift imm7 7 15)
  .|. (codeTag regT2 `shiftL` 10)
  .|. (codeTag regN `shiftL` 5)
  .|. (codeTag regT)
encode (STR NoOffset regN regT imm12) = zero32
  .|. ((0b1011100100 :: Word32) `shiftL` 22)
  .|. (bitMode `shiftL` 31) -- 32 bit mode
  .|. (clampShift imm12 12 10)
  .|. (codeTag regN `shiftL` 5)
  .|. (codeTag regT)
encode (STR postIndex regN regT imm9) = zero32 -- postIndex in [PreIndex, PostIndex]
  .|. ((0b10111000000 :: Word32) `shiftL` 21)
  .|. (bitMode `shiftL` 31) -- 32 bit mode
  .|. (clampShift imm9 9 12)
  .|. (codeTag postIndex `shiftL` 10)
  .|. (codeTag regN `shiftL` 5)
  .|. (codeTag regT)
encode (LDR NoOffset regN regT imm12) = zero32
  .|. ((0b1011110101 :: Word32) `shiftL` 22)
  .|. (bitMode `shiftL` 30) -- 32 bit mode
  .|. (clampShift imm12 12 10)
  .|. (codeTag regN `shiftL` 5)
  .|. (codeTag regT)
encode (LDR postIndex regN regT imm9) = zero32
  .|. ((0b10111100010 :: Word32) `shiftL` 21)
  .|. (bitMode `shiftL` 31) -- 32 bit mode
  .|. (clampShift imm9 9 12)
  .|. (codeTag postIndex `shiftL` 10)
  .|. (codeTag regN `shiftL` 5)
  .|. (codeTag regT)
encode (BCOND condFlag imm19)
  | imm19 > 0 = 
      ((0b01010100 :: Word32) `shiftL` 24)
  .|. (clampShift imm19 19 5)
  .|. (codeTag condFlag)
  -- TODO toss this clause
  | otherwise =
      ((0b01010100 :: Word32) `shiftL` 24)
  .|. (clampShift imm19 19 5)
  .|. (codeTag condFlag)
encode (CMPI reg imm12) = zero32
  .|. ((0b01110001 :: Word32) `shiftL` 24)
  .|. (clampShift imm12 12 10)
  .|. (codeTag reg `shiftL` 5)
  .|. (createMask 5)
encode (B imm26) = zero32
  .|. ((0b00101 :: Word32) `shiftL` 26)
  .|. (clampShift imm26 26 0)
encode (BL imm26) = zero32
  .|. ((0b100101 :: Word32) `shiftL` 26)
  .|. (clampShift imm26 26 0)
encode (BLR reg) = zero32 
  .|. ((0b_1101011_0_0_01_11111 :: Word32) `shiftL` 16)
  .|. (codeTag reg `shiftL` 5)
encode (BR reg) = zero32 
  .|. ((0b_1101011_0000_11111 :: Word32) `shiftL` 16)
  .|. (codeTag reg `shiftL` 5)
encode (RET reg) = zero32
  .|. ((0b_1101011_0_0_10_11111 :: Word32) `shiftL` 16)
  .|. ((codeTag $ fromMaybe LR reg ) `shiftL` 5)
encode (MOV_reg regMoving regDest) = zero32
  .|. (bitMode `shiftL` 31)
  .|. ((0b_01_01010_00_0 :: Word32) `shiftL` 21)
  .|. (codeTag regMoving `shiftL` 16)
  .|. ((0b_11111 :: Word32) `shiftL` 5)
  .|. codeTag regDest
encode (MOV movk keepbits reg imm16) = zero32 -- (MOVK/MOVZ)
  .|. (bitMode `shiftL` 31) -- 32 bit mode
  .|. (0b10100101 `shiftL` 23)  -- opc + data magic bits
  .|. (codeTag keepbits `shiftL` 29)
  .|. (codeTag movk  `shiftL` 21)
  .|. (clampShift (fromInteger . toInteger $ imm16) 16 5)
  .|. (codeTag reg)
encode (ADD carryFlag trg src1 src2) = zero32 -- ADD target = src + src, impl
  .|. (bitMode `shiftL` 31) -- 32 bit mode
  .|. (codeTag carryFlag `shiftL` 29)
  .|. (0b01011001 `shiftL` 21)
  .|. (codeTag src1 `shiftL` 16)
  .|. (codeTag src2 `shiftL` 5)
  .|. (codeTag trg)
encode (SUB carryFlag trg src1 src2) = zero32
  .|. (bitMode `shiftL` 31) -- 32 bit mode
  .|. (codeTag carryFlag `shiftL` 29)
  .|. (0b1001011001 `shiftL` 21)
  .|. (codeTag src1 `shiftL` 16)
  .|. (codeTag src2 `shiftL` 5)
  .|. (codeTag trg)
encode (NOP) = 0xD503201F
encode _ = zero32

codeTag :: Enum a => a -> Word32
codeTag = fromIntegral . fromEnum


-- | maskLen must be g.t. 0 or else bad things happen
clampShift :: Int -> Int -> Int -> Word32
clampShift inp maskLen shift = ((fromInteger . toInteger $ inp) .&. (createMask maskLen)) `shiftL` shift

createMask :: Int -> Word32
createMask n = foldl1 (.|.) $ fmap bit [0..(n-1)]

hex :: (Integral a, Show a) => a -> String
hex x = showHex x ""

-- Define our monad, ASM
data JITMem = JITMem
 { instr :: [Instr]
 , mach   :: [Word32]
 , memptr :: Word32
 , memoff :: Word32
 } deriving (Eq, Show)

initState :: Word32 -> JITMem
initState start = JITMem
  { instr = []
  , mach   = []
  , memptr = start
  , memoff = 0
  }

type ASM a = StateT JITMem (Except String) a

emit :: Instr -> ASM ()
emit i = modify $ \s -> s
  { mach = (mach s) ++ [encode i]
  , memoff = memoff s + 1
  , instr  = (instr s) ++ [i]
  }


assemble :: Ptr a -> ASM b -> Either String JITMem
assemble start asm = runExcept $ execStateT asm (initState ptr)
  where
    ptr = heapPtr start


moveImm32 :: Reg -> Word32 -> ASM ()
moveImm32 reg word = do
  emit $ MOV ClearBits Mk0 reg lower
  emit $ MOV KeepBits Mk16 reg upper
  where
    x :: Int
    x = fromInteger . toInteger $ word
    mask :: Int
    mask  = fromInteger . toInteger $ createMask 16 `shiftL` 16
    upper :: Word16
    upper = fromInteger . toInteger $ ((x .&. mask) `shiftR` 16)
    lower :: Word16
    lower = fromInteger . toInteger $ clampShift x 16 0

