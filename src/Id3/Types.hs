module Id3.Types
  (someFunc
  ) where

import qualified Data.Map as Map

someFunc = putStrLn "Hello World"

-- | Type representing an id3 frame per the spec
type Frame = Map.Map String String

-- | Data representing an id3 tag, with frames and information
-- | from the header
data Id3 = Id3 { version :: Int
               , minorVersion :: Int
               , id3Size :: Int
               , compressed :: Bool
               , unsync :: Bool
               , frames :: [Frame]
               } deriving (Show) -- for debug
