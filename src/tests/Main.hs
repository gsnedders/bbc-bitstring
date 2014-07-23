module Main where

import Bitstring.Internal

import Text.Printf
import Test.QuickCheck

main  = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

tests :: [(String, IO ())]
tests = []
