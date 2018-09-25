module Id3.Types
where

import qualified Data.Map as Map
import Data.Word
import qualified Data.ByteString.Lazy as B
-- import Data.ByteString.Lazy.UTF8

-- | Type representing an id3 frame per the spec
-- | Frame <id> <flags> <size in bytes> <data>
data Frame = Frame B.ByteString Word16 Int B.ByteString
instance Show Frame where
  show (Frame id flags size d) = "(Frame " ++ show id ++ ": size=" ++ show size ++ " flags="++ show flags ++")"

-- | Data representing an id3 tag, with frames and information
-- | from the header
data Id3 = Id3 { version :: Integer
               , minorVersion :: Integer
               , id3Size :: Integer
               , extHeader :: Bool
               , experiment :: Bool
               , unsync :: Bool
               , frames :: [Frame]
               } deriving (Show) -- for debug
