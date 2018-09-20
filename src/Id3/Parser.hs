{-# LANGUAGE OverloadedStrings #-}
module Id3.Parser
  where

import Id3.Types

import Control.Monad
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word

id3HeaderCode = B.unpack ("ID3" :: B.ByteString)

detectId3 :: Get (Bool)
detectId3 = do
  identifier <- replicateM 3 getWord8
  return (identifier == id3HeaderCode)

deserializeHeader :: Get Id3
deserializeHeader = do
  identifier <- replicateM 3 getWord8
  majorVersion <- toInteger <$> getWord8
  minorVersion <- toInteger <$> getWord8
  flags <- getWord8
  unsync <- return $ testBit flags 7
  compr <- return $ testBit flags 6
  size <- toInteger <$> getWord32be
  return $ Id3 majorVersion minorVersion size compr unsync []
  
