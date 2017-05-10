module Main where

import Lib

import Prelude hiding (getContents, lines)
import Data.ByteString.Char8 (getContents, lines, ByteString)
main :: IO ()
main = do
  text : pat : _  <- fmap lines getContents
  putStrLn $ show (Lib.patternCount text pat)
