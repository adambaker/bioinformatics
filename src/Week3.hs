module Week3(motifDist, motifEnum, median, kMers, profileCol, ProfileCol(..),
Profile, window, mostProbableStr) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Week2 (hamming)
import FreqArray(intToPattern)
import Data.Word (Word)

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

data ProfileCol = ProfileCol {a :: Double, c :: Double, g :: Double, t :: Double}
type Profile = [ProfileCol]

profileCol :: (Word, Word, Word, Word) -> ProfileCol
profileCol (a', c', g', t') = let
  toDouble :: Word -> Double
  toDouble = toEnum . fromEnum
  tot = toDouble $ a' + c' + g' + t'
  [a, c, g, t] = map ((/tot) . toDouble) [a', c', g', t']
  in ProfileCol a c g t

profileColChars :: [Char] -> ProfileCol
profileColChars = profileCol . foldl incChar (0, 0, 0, 0) where
  incChar :: (Word, Word, Word, Word) -> Char -> (Word, Word, Word, Word)
  incChar (a, c, g, t) 'A' = (succ a, c, g, t)
  incChar (a, c, g, t) 'C' = (a, succ c, g, t)
  incChar (a, c, g, t) 'G' = (a, c, succ g, t)
  incChar (a, c, g, t) 'T' = (a, c, g, succ t)

formProfile :: [ByteString] -> Profile
formProfile strs = profileColChars (map BS.head strs) : formProfile (map BS.tail strs)

pChar :: ProfileCol -> Char -> Double
pChar prof 'A' = a prof
pChar prof 'C' = c prof
pChar prof 'G' = g prof
pChar prof 'T' = t prof

pStr :: Profile -> ByteString -> Double
pStr profile str = product $ zipWith pChar profile (BS.unpack str)

window :: Int -> ByteString -> [ByteString]
window k str
  | BS.length str == k = [str]
  | otherwise = BS.take k str : window k (BS.tail str)

mostProbableStr :: Profile -> ByteString -> ByteString
mostProbableStr prof = snd . maximum . map (\s -> (pStr prof s, s)) . window (length prof)
