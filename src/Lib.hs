module Lib
    ( patternCount
    ) where

import Prelude hiding (tail, length)
import Data.ByteString (ByteString, tail, length, isPrefixOf)

patternCount :: ByteString -> ByteString -> Int
patternCount text pattern = let
  patLen = length pattern
  loop :: Int -> ByteString -> Int
  loop count text'
    | length text' < patLen = count
    | otherwise = loop (count + fromEnum (isPrefixOf pattern text')) (tail text')
  in loop 0 text
