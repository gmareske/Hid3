module Main where

import Id3.Parser

import qualified Data.ByteString.Lazy as B
import Data.Binary.Get
import Data.Word

main :: IO ()
main = do
  input <- B.getContents
  print $ runGet deserializeHeader input
