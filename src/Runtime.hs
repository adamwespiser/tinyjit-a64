{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}

module Runtime where

import ASM

import Control.Exception
import Data.Bits
import Data.Dynamic
import Data.Functor
import Data.Word
import Foreign.Ptr
import Foreign.C.String
import Foreign.C.Types
import Foreign.Marshal.Utils
import Foreign.ForeignPtr
import System.Posix.DynamicLinker
import System.Posix.Types
import Unsafe.Coerce
import qualified Data.Vector.Storable as V
import qualified Data.Vector.Storable.Mutable as VM

-- inspired by: https://github.com/sdiehl/tinyjit/blob/master/src/JIT.hs

newtype MmapOption = MmapOption CInt
 deriving (Eq, Show, Ord, Num, Bits)

newtype ProtOption = ProtOption CInt
  deriving (Eq, Show, Ord, Num, Bits)

instance Semigroup ProtOption where -- semigroup append is bitwise OR
  (ProtOption a) <> (ProtOption b) = ProtOption (a .|. b)

pExec = ProtOption 0x01
pWrite = ProtOption 0x02
mmapAnon = MmapOption 0x20
mmapPrivate = MmapOption 0x02

allocateMemory :: CSize -> IO (Ptr Word8)
allocateMemory size = mmap nullPtr size pflags mflags (-1) 0
  where
    pflags = (ProtOption 0x04) .|. (ProtOption 0x02) .|. (ProtOption 0x01) -- r/w
    mflags = (MmapOption 0x20) .|. (MmapOption 0x02) -- Anon | Private

data MmapException = MmapException
  deriving (Show, Typeable)

instance Exception MmapException

-- FFI to mmap
-- https://man7.org/linux/man-pages/man2/mmap.2.html
foreign import ccall unsafe "sys/mman.h mmap"
  mmap_ffi
    :: Ptr ()
    -> CSize
    -> ProtOption
    -> MmapOption
    -> Fd -- fd / file descriptor
    -> COff -- off_t t_offset
    -> IO (Ptr Word8)

mmap
  :: Ptr ()
  -> CSize
  -> ProtOption
  -> MmapOption
  -> Fd -- fd / file descriptor
  -> COff -- off_t t_offset
  -> IO (Ptr Word8)
mmap addr csize protOpt mmapOpt fd coff = do
  ptr <- mmap_ffi addr csize protOpt mmapOpt fd coff
  if (ptr == intPtrToPtr (-1)) then throwIO MmapException else pure ()
  return ptr

extern :: String -> IO Word32
extern name = do
  dl <- dlopen "" [RTLD_LAZY, RTLD_GLOBAL]
  dlsym dl name <&> heapPtr . castFunPtrToPtr

heapPtr :: Ptr a -> Word32
heapPtr = fromIntegral . ptrToIntPtr

-- ARM a64 has 32bit fixed width instructions
codePtr :: [Word8] -> IO (VM.IOVector Word8)
codePtr = V.thaw . V.fromList

vecPtr :: VM.Storable a => VM.MVector s a -> ForeignPtr a
vecPtr = fst . VM.unsafeToForeignPtr0

asciz :: [Char] -> IO Word32
asciz str = do
  ptr <- newCString $ str ++ "\n"
  return . heapPtr $ ptr

jit :: Ptr Word8 -> [Word32] -> IO (IO Int)
jit mem machCode = do
  let machCodeBytes = concat $ fmap toByteArray machCode
  code <- codePtr machCodeBytes
  withForeignPtr (vecPtr code) $ \ptr -> do
    copyBytes mem ptr $ (length machCodeBytes) * 8 -- what's the "6" represent here? s.d. used (8*6)
  return $ getFunction mem

foreign import ccall "dynamic"
  mkFun :: FunPtr (IO Int) -> IO Int

getFunction :: Ptr Word8 -> IO Int
getFunction mem = do
 let fptr = unsafeCoerce mem :: FunPtr (IO Int)
 mkFun fptr

