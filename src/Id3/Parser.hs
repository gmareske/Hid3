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
  extHeader <- return $ testBit flags 6 -- not supported
  experiment <- return $ testBit flags 5 -- not supported
  size <- toInteger <$> getWord32be
  if extHeader then deserializeExtHeader else return ()
  return $ Id3 majorVersion minorVersion size extHeader experiment unsync []

-- | Parsing the extended header 
deserializeExtHeader :: Get ()
deserializeExtHeader = do
  headerSize <- fromIntegral <$> toInteger <$> getWord32be
  extFlags <- getWord16be
  paddingSize <- getWord32be
  if testBit extFlags 15 then do
    crc <- getWord32be
    return ()
  else return ()
  return ()
  
readFrameHeader :: Get Frame
readFrameHeader = do
  frameId <- B.pack <$> replicateM 4 getWord8 -- better conversiont to ByteString
  size <- toInteger <$> getWord32be
  flags <- replicateM 2 getWord8
  frameData <- B.pack <$> replicateM (fromIntegral size) getWord8 
  return $ Frame frameId size frameData
