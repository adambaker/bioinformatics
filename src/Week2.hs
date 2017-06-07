module Week2(hamming, minSkew, approxMatches, approxCount, neighbors,
approxPatCount, approxRevCompCount)
where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import FreqArray(countFreqs, mostFrequent)
import Data.Array.IArray (assocs)
import Lib(revComp)

hamming :: ByteString -> ByteString -> Int
hamming t1 t2 = sum $ BS.zipWith (\x y -> fromEnum (x /= y)) t1 t2

approxMatches :: ByteString -> ByteString -> Int -> [Int]
approxMatches pattern text dist = let
  len = BS.length pattern
  loop :: ByteString -> Int -> [Int] -> [Int]
  loop rest idx found
    | BS.length rest < len = found
    | hamming pattern (BS.take len rest) <= dist = loop (BS.tail rest) (idx + 1) (idx:found)
    | otherwise = loop (BS.tail rest) (idx +1) found
  in reverse $ loop text 0 []

approxCount p t d = length $ approxMatches p t d

minSkew :: ByteString -> [Int]
minSkew text = snd . fst $ BS.foldl step ((0, [0]), (0, 0)) text
  where
    step ((i, minI), (best, curr)) 'C'
      | curr - 1 == best = ((i+1, i+1:minI), (best, best))
      | curr == best = ((i+1, [i+1]), (curr-1, curr-1))
      | otherwise = ((i+1, minI), (best, curr-1))
    step ((i, minI), (best, curr)) 'G' = ((i+1, minI), (best, curr+1))
    step ((i, minI), scores) _ = ((i+1, minI), scores)

bases = ['A', 'C', 'G', 'T']
strNeighbors :: ByteString -> Int -> [String]
strNeighbors pat 0 = [BS.unpack pat]
strNeighbors pat d
  | BS.length pat == 1 = map return bases
  | otherwise = do
    let rest = BS.tail pat
    suffix <- strNeighbors rest d 
    if hamming (BS.pack suffix) rest == d
      then [BS.head pat : suffix]
      else map (:suffix) bases

neighbors :: Int -> ByteString -> [ByteString]
neighbors d pat = map BS.pack $ strNeighbors pat d

approxPatCount :: Int -> Int -> ByteString -> [ByteString]
approxPatCount k d genome = mostFrequent $ countFreqs (neighbors d) k genome

approxRevCompCount :: Int -> Int -> ByteString -> [ByteString]
approxRevCompCount k d text = mostFrequent $ countFreqs neighRevComp k text
  where neighRevComp t = neighbors d t ++ neighbors d (revComp t)
