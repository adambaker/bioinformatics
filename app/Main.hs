{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as BS
import Week3 (mostProbableStr, Profile, ProfileCol(..))
import Control.Applicative (ZipList(..), (<*>), (<$>))

main :: IO ()
main = do
  dna : _ : profstrs <- fmap BS.lines BS.getContents
  let parseDoubles = (map (read . BS.unpack) . BS.words :: BS.ByteString -> [Double])
  let [as, cs, gs, ts] = (map (ZipList . parseDoubles) profstrs :: [ZipList Double])
  let profile = ((getZipList $ ProfileCol <$> as <*> cs <*> gs <*> ts) :: Profile)
  print $ mostProbableStr profile dna
