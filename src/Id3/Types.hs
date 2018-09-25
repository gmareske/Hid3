module Id3.Types
where

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as B

-- | Type representing an id3 frame per the spec
-- | Frame <id> <size in bytes> <data>
data Frame = Frame B.ByteString Integer B.ByteString deriving (Show)

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
