module Bitstring.Internal where

import Data.Bits (popCount, shiftL, (.|.))
import Data.Char (chr, ord)
import Data.Word (Word8)
import qualified Data.ByteString as B

data MByteSequence = MByteSequence [Word8] -- a sequence of map bytes (len 0-15)

data Atom = TerminationAtom |
            NonOffsetAtom { nonOffsetGapLength :: Integer -- the number of gap bytes (max 2**53-1)
                          , nonOffsetGapValue :: Bool -- the value of the gap bytes
                          , followingMapBytes :: MByteSequence -- a sequence of following map bytes (len 1-15)
                          } |
            OffsetAtom { offsetGapLength :: Integer -- the number of gap bytes (max 2**53-1)
                       , offsetGapValue :: Bool -- the value of the gap bytes and the majority of the offset byte
                       , offsetBit :: Int -- the bit with a different value in the offset byte (0-7)
                       }

getAtomType :: Atom -> Int
getAtomType TerminationAtom = 1
getAtomType (NonOffsetAtom len _ _)
  | len <= 3 = fromInteger len
  | len > 3  = 4
getAtomType (OffsetAtom len True _)
  | len <= 3 = 7
getAtomType (OffsetAtom len _ _)
  | len > 3  = 6
getAtomType (OffsetAtom len False _)
  | len <= 3 = 5

getFillField :: Atom -> Int
getFillField TerminationAtom          = 0
getFillField (NonOffsetAtom _ val _)  = if val then 1 else 0
getFillField (OffsetAtom len True _)  = if len > 3 then 1 else fromInteger len
getFillField (OffsetAtom len False _) = if len > 3 then 0 else fromInteger len

getDataField :: Atom -> Int
getDataField TerminationAtom = 0
getDataField (NonOffsetAtom _ val (MByteSequence mbytes))
  | length mbytes == 1 &&
    mbytes !! 0 == (if val then 0 else 0xFF) = 0
  | otherwise                                = length mbytes
getDataField (OffsetAtom _ _ bit) = bit

serialize :: Atom -> Word8
serialize TerminationAtom = 0
serialize (NonOffsetAtom x y z) = fromIntegral $ (getAtomType all) `shiftL` 6 .|.
                                                 (getFillField all) `shiftL` 4 .|.
                                                 (getDataField all)
  where all = NonOffsetAtom x y z
serialize (OffsetAtom x y z) = fromIntegral $ (getAtomType all) `shiftL` 6 .|.
                                              (getFillField all) `shiftL` 3 .|.
                                              (getDataField all)
  where all = OffsetAtom x y z
