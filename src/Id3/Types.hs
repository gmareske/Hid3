module Id3.Types
where

import qualified Data.Map as Map

-- | Type representing an id3 frame per the spec
type Frame = Map.Map String String

-- | Data representing an id3 tag, with frames and information
-- | from the header
data Id3 = Id3 { version :: Integer
               , minorVersion :: Integer
               , id3Size :: Integer
               , compressed :: Bool
               , unsync :: Bool
               , frames :: [Frame]
               } deriving (Show) -- for debug

