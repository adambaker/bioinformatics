{-# LANGUAGE RankNTypes #-}
module FreqArray(FreqArray, new, inc, toStr, countFreqs, mostFrequent,
intToPattern)
where

import Prelude hiding (max)
import Data.Array.IArray (elems, assocs)
import Data.Array.Unboxed (UArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Control.Monad.ST (ST, runST)
import Data.List (intercalate)
import Debug.Trace (traceShowId)

data FreqArray s = FreqArray {patLen :: Int, freqs :: STUArray s Int Int}
type FreqST s = ST s (FreqArray s)

new :: Int -> FreqST s
new k = FreqArray k `fmap` newArray (0, (fromEnum (4 ** fromIntegral k))-1) 0

inc :: FreqArray s -> ByteString -> ST s ()
inc (FreqArray k a) kmer = do
  let i = patternToInt kmer
  count <- readArray a i
  writeArray a i $ count+1

countFreqs :: (ByteString -> [ByteString]) -> Int -> ByteString -> FreqST s
countFreqs neighbors k text = do
  freqs <- new k
  let loop t
        | BS.length t < k = return ()
        | otherwise = mapM_ (inc freqs) (neighbors $ BS.take k t) >> loop (BS.tail t)
  loop text
  return freqs

toStr :: (forall s. FreqST s) -> String
toStr stA = (intercalate " " . map show . elems) (runSTUArray $ fmap freqs stA)

mostFrequent :: (forall s. FreqST s) -> [ByteString]
mostFrequent stA = let
  toPat :: Int -> ByteString
  toPat = intToPattern (runST $ fmap patLen stA)
  step :: (Int, [Int]) -> (Int, Int) -> (Int, [Int])
  step last@(lastCount, is) (i, count)
    | count == lastCount = (count, i:is)
    | count > lastCount = (count, [i])
    | otherwise = last
  in map toPat . snd $ foldl step (0, []) (assocs $ runSTUArray $ fmap freqs stA)

baseToInt :: Char -> Int
baseToInt 'A' = 0
baseToInt 'C' = 1
baseToInt 'G' = 2
baseToInt 'T' = 3

intToBase :: Int -> Char
intToBase 0 = 'A'
intToBase 1 = 'C'
intToBase 2 = 'G'
intToBase 3 = 'T'

patternToInt :: ByteString -> Int
patternToInt = BS.foldl' (\x c -> x*4 + baseToInt c) 0

intToPattern :: Int -> Int -> ByteString
intToPattern k x = let
  next:: (Int, Int) -> Maybe (Char, (Int, Int))
  next (0, _) = Nothing
  next (k', n) = let (rest, b) = n `divMod` 4 in Just (intToBase b, (k'-1, rest))
  in BS.reverse $ BS.unfoldr next (k, x)
