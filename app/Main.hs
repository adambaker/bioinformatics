module Main where

import FreqArray (toStr, countFreqs)

import Prelude hiding (getContents, lines)
import Data.ByteString.Char8 (getContents, lines, ByteString, pack, unpack)
import Data.Set (size)
import Data.List (intercalate)
main :: IO ()
main = do
  text : k : _ <- fmap lines getContents
  putStrLn $ toStr $ countFreqs return (read $ unpack k) text
