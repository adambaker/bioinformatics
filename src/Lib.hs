{-# LANGUAGE RankNTypes #-}
module Lib(
    patternCount,
    freqWords,
    revComp,
    clumps,
    indexes
    ) where

import Prelude hiding (tail, length, take, drop)
import Data.ByteString.Char8 (ByteString, tail, length, isPrefixOf, take, pack,
    unpack)
import Data.ByteString.Char8 as BS hiding (head, map, takeWhile, reverse)
import Data.List (sortBy)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set as Set hiding (map)
import Data.HashTable.Class as H
import Data.HashTable.ST.Basic as HT
import Data.STRef (STRef, newSTRef, modifySTRef, readSTRef)
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
revComp = pack . BS.foldl (\str c -> (comp c):str) []

indexes :: ByteString -> ByteString -> [Integer]
indexes pat text = reverse $ loop 0 text []
  where
    lenPat = length pat
    loop :: Integer ->  ByteString -> [Integer] -> [Integer]
    loop x text' idxs
      | length text' < lenPat = idxs
      | otherwise = loop (x+1) (tail text') (if isPrefixOf pat text' then x:idxs else idxs)

type ClumpSet s = STRef s (Set ByteString)
clumps :: Int -> Int -> Int -> ByteString -> Set ByteString
clumps k window times text = runST $ do
  counts <- H.new
  found <- newSTRef Set.empty
  init counts found 0
  scan counts found text
  readSTRef found
  where
    toCurr = window - k
    init :: Counts s -> ClumpSet s -> Int -> ST s ()
    init counts found t =  do
      incAndMark counts found (take k . drop t $ text)
      if t + k == window then return () else init counts found (t+1)
    scan :: Counts s -> ClumpSet s -> ByteString -> ST s ()
    scan counts found text = do
      dec counts (take k text)
      incAndMark counts found (take k . drop toCurr $ text)
      if length text == window then return () else scan counts found (tail text)
    incAndMark :: Counts s -> ClumpSet s -> ByteString -> ST s ()
    incAndMark counts found str = do
      curr <- fmap (fromMaybe 0) (H.lookup counts str)
      let next = curr + 1
      H.insert counts str next
      if next == times then modifySTRef found (Set.insert str) else return ()

dec :: Counts s -> ByteString -> ST s ()
dec counts str = do
  curr <- fmap (fromMaybe (error ("dec " ++ unpack str ++ " not there")))
    (H.lookup counts str)
  H.insert counts str (curr - 1)
