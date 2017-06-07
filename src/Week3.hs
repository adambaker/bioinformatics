module Week3(motifDist, motifEnum, median, kMers) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Week2 (hamming)
import FreqArray(intToPattern)

dist :: ByteString -> ByteString -> Int
dist pat dna
  |BS.length dna < BS.length pat = dist dna pat
  |otherwise = loop maxBound dna where
    loop :: Int -> ByteString -> Int
    loop minDist text = if BS.length text == BS.length pat
      then min minDist (hamming text pat)
      else loop (min minDist (hamming text pat)) (BS.tail text)

motifDist :: ByteString -> [ByteString] -> Int
motifDist pat = sum . map (dist pat)

kMers :: Int -> [ByteString]
kMers k = map (intToPattern k) [0..(4^k-1)]

motifEnum :: Int -> Int -> [ByteString] -> [ByteString]
motifEnum k d dna = filter allWithinD $ kMers k where
  withinD pat text = dist pat text <= d
  allWithinD pat = all (withinD pat) dna

median :: Int -> [ByteString] -> ByteString
median k dna = snd $ foldl step (maxBound, "") $ kMers k where
  step :: (Int, ByteString) -> ByteString -> (Int, ByteString)
  step (minDist, minPat) pat = let d = motifDist pat dna in
    if d < minDist then (d, pat) else (minDist, minPat)
