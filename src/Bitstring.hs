module Bitstring where

import Bitstring.Internal

data CompressedBitstring = CompressedBitstring [Atom]

--encode :: [Bool] -> CompressedBitstring

--decode :: [Bool] -> CompressedBitstring
