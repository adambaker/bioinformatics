{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Char8 as BS
import Week3 (motifEnum)

main :: IO ()
main = do
  kd : dnas <- fmap BS.lines BS.getContents
  let k : d : _ = map (read . BS.unpack) $ BS.words kd
  BS.putStr $ BS.intercalate " " $  motifEnum k d dnas
