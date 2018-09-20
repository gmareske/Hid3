{-# LANGUAGE OverloadedStrings #-}
module Id3.Parser
  where

import Control.Monad
import Data.Binary.Get
import qualified Data.ByteString.Lazy as B
import Data.Word

id3HeaderCode = B.unpack ("ID3" :: B.ByteString)

detectId3 :: Get (Bool)
detectId3 = do
  identifier <- replicateM 3 getWord8
  return (identifier == id3HeaderCode)

deserializeHeader :: Get (Word8, Word8, Word8, Word32)
deserializeHeader = do
  identifier <- replicateM 3 getWord8
  majorVersion <- getWord8
  minorVersion <- getWord8
  flags <- getWord8
  size <- getWord32be
  return (majorVersion, minorVersion, flags, size) 
