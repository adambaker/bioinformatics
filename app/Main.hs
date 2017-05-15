module Main where

import Lib

import Prelude hiding (getContents, lines)
import Data.ByteString.Char8 (getContents, lines, ByteString, pack)
import Data.List (intercalate)
main :: IO ()
main = getContents >>= 
  putStrLn . intercalate " " . map show . indexes (pack "CTTGATCAT")
