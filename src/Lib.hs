{-# LANGUAGE RankNTypes #-}
module Lib(
    patternCount,
    freqWords,
    revComp,
    indexes
    ) where

import Prelude hiding (tail, length, take, foldl)
import Data.ByteString.Char8 (ByteString, tail, length, isPrefixOf, take, foldl, pack)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.HashTable.Class as H
import Data.HashTable.ST.Basic as HT
import Control.Monad.ST (ST, runST)

type Counts s = HT.HashTable s ByteString Int
type CountsST s = ST s (Counts s)

patternCount :: ByteString -> ByteString -> Int
patternCount text pattern = let
  patLen = length pattern
  loop :: Int -> ByteString -> Int
  loop count text'
    | length text' < patLen = count
    | otherwise = loop (count + fromEnum (isPrefixOf pattern text')) (tail text')
  in loop 0 text

freqWords :: ByteString -> Int -> [ByteString]
freqWords text k = mostFreq $ Lib.toList (H.new >>= countAll text k)

countAll :: ByteString -> Int -> Counts s -> CountsST s
countAll text k counts
  | length text < k = return counts
  | otherwise = inc counts (take k text) >>= countAll (tail text) k 

inc :: Counts s -> ByteString -> CountsST s
inc counts str = do
  curr <- fmap (fromMaybe 0) (H.lookup counts str)
  H.insert counts str (curr + 1)
  return counts

toList :: (forall s. CountsST s) -> [(ByteString, Int)]
toList counts = runST $ counts >>= H.toList

mostFreq :: [(ByteString, Int)] -> [ByteString]
mostFreq [] = []
mostFreq xs = let
  sorted = sortBy (\(_, x) (_, y) -> y `compare` x) xs 
  highest = snd (head sorted)
  in map fst $ takeWhile (\x -> (snd x) == highest) sorted

comp :: Char -> Char
comp 'A' = 'T'
comp 'T' = 'A'
comp 'C' = 'G'
comp 'G' = 'C'

revComp :: ByteString -> ByteString
revComp = pack . foldl (\str c -> (comp c):str) []

indexes :: ByteString -> ByteString -> [Integer]
indexes pat text = reverse $ loop 0 text []
  where
    lenPat = length pat
    loop x text' idxs
      | length text' < lenPat = idxs
      | otherwise = loop (x+1) (tail text') (if isPrefixOf pat text' then x:idxs else idxs)
