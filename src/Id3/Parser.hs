{-# LANGUAGE OverloadedStrings #-}
module Id3.Parser
  where

import Id3.Types


import Control.Monad
import Data.Binary.Get
import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.Word
import Text.Printf

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
  extHeader <- return $ testBit flags 6
  experiment <- return $ testBit flags 5
  size <- toInteger <$> getWord32be
  if extHeader then deserializeExtHeader else return ()
  return (Id3 majorVersion minorVersion size extHeader experiment unsync)
    <*> deserializeFrames (fromIntegral size)
  

-- | Parsing the extended header
-- This procedure is only called from deserializeHeader if the
-- extended header flag is set  
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

deserializeFrames :: Int -> Get [Frame]
deserializeFrames left = do
  parseFrame left []
--   replicateM 8 (parseOneFrame left [])

parseFrame :: Int -> [Frame] -> Get [Frame]
parseFrame left frames = do
  if left > 0 then do
    id <- B.pack <$> replicateM 4 getWord8
    -- this check is done because often times id3 tags end with padding
    -- which is done so id3 tags can be destructively expanded
    if (head $ B.unpack id) == (0 :: Word8) then do
      return $ frames -- we hit padding (id's bytes are always not null)
      else do
      size <- toInteger <$> getWord32be
      flags <- getWord16be
      -- labeled for debugging 
      frameData <- label (printf "trying to get %d bytes with %d left, tag id=%s flags=%s" size left (show id) (show flags)) $ getLazyByteString (fromIntegral size)
      parseFrame (left - (10 + (fromIntegral size))) $ (Frame id flags (fromIntegral size) frameData) : frames
    else return $ frames

parseOneFrame :: Int -> [Frame] -> Get Frame
parseOneFrame x frames = do
  id <- B.pack <$> replicateM 4 getWord8
  size <- toInteger <$> getWord32be
  flags <- getWord16be
  frameData <- label (printf "trying to get %d bytes with %d left" size x) $ getLazyByteString (fromIntegral size)
  return $ (Frame id flags (fromIntegral size) frameData)
