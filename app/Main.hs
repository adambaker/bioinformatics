module Main where

import Lib

import Prelude hiding (getContents, lines)
import Data.ByteString.Char8 (getContents, lines, ByteString, pack)
import Data.Set (size)
import Data.List (intercalate)
main :: IO ()
main = getContents >>= 
  putStrLn . show . size . clumps 9 500 3
