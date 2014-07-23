module Bitstring.Internal where

import Data.Bits (popCount, shiftL, shiftR, (.|.), (.&.))
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
  | len <= 3  = fromInteger len
  | otherwise = 4
getAtomType (OffsetAtom len True _)
  | len <= 3 = 7
getAtomType (OffsetAtom len False _)
  | len <= 3 = 5
getAtomType (OffsetAtom _ _ _)
  | otherwise  = 6

getFillField :: Atom -> Int
getFillField TerminationAtom          = 0
getFillField (NonOffsetAtom _ val _)  = if val then 1 else 0
getFillField (OffsetAtom len True _)  = if len > 3 then 1 else fromInteger len
getFillField (OffsetAtom len False _) = if len > 3 then 0 else fromInteger len

getDataField :: Atom -> Int
getDataField TerminationAtom = 0
getDataField (NonOffsetAtom _ val (MByteSequence mbytes))
  | length mbytes == 1 &&
    head mbytes == (if val then 0 else 0xFF) = 0
  | otherwise                                = length mbytes
getDataField (OffsetAtom _ _ bit) = bit

serialize :: Atom -> [Word8]
serialize TerminationAtom = [0]
serialize atom@(NonOffsetAtom _ _ _) = [serializeControlByte 4 atom]
serialize atom@(OffsetAtom _ _ _) = [serializeControlByte 3 atom]

serializeControlByte :: Int -> Atom -> Word8
serializeControlByte dFieldWidth atom = fromIntegral $ (getAtomType atom) `shiftL` 6 .|.
                                                       (getFillField atom) `shiftL` dFieldWidth .|.
                                                       (getDataField atom)

getDFieldWidth :: Integral a => Word8 -> a
getDFieldWidth atomType
  | atomType <= 4 = 4
  | otherwise     = 3

-- parseControlByte :: Word8 -> (Word8, Word8, Word8)
-- parseControlByte cbyte = (fromIntegral tField,
--                           fromIntegral $ cbyte `shiftR` dFieldWidth .&. 1,
--                           fromIntegral $ cbyte .&. (1 `shiftR` dFieldWidth) - 1)
--   where tField = cbyte `shiftR` 6
--         dFieldWidth = (getDFieldWidth tField)

-- parseGBytes :: [Word8] -> (Integer, [Word8])
-- parseGBytes (x:xs) = ((sum $ map (toInteger . popCount) bytes) `shiftR` 4,
--                       drop (fromIntegral x) xs)
--   where bytes = take (fromIntegral x) xs

-- parse :: [Word8] -> Atom
-- parse (cbyte:rest) = case (parseControlByte cbyte) of
--   (0, 0, 0) -> TerminationAtom
--   (1, fField, dField) -> NonOffsetAtom 1 (if fField == 1 then True else False) (MByteSequence rest)
--   (2, fField, dField) -> NonOffsetAtom 2 (if fField == 1 then True else False) (MByteSequence rest)
--   (3, fField, dField) -> NonOffsetAtom 3 (if fField == 1 then True else False) (MByteSequence rest)
--   (4, fField, dField) -> NonOffsetAtom gapSize (if fField == 1 then True else False) (MByteSequence mbytes)
--     where (gapSize, mbytes) = parseGBytes rest
--   (5, fField, dField) -> OffsetAtom fField False


data CategorisedByte = TrueByte | FalseByte | OffsetByte | MapByte deriving (Show, Eq)

categorize :: [Bool] -> CategorisedByte
categorize xs
  | all id xs  = TrueByte
  | all not xs = FalseByte
  | (sum $ map (\x -> if x then 1 else 0) xs) == 1 = OffsetByte
  | (sum $ map (\x -> if x then 1 else 0) xs) == 7 = OffsetByte
  | otherwise = MapByte

grouping :: CategorisedByte -> CategorisedByte -> Bool
grouping OffsetByte MapByte = True
grouping MapByte OffsetByte = True
grouping x y                = x == y

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n xs
  | n > 0     = let (firstChunk, rest) = splitAt n xs in firstChunk : chunk n rest
  | otherwise = error "Non-positive n"
