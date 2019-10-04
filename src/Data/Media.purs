module Halogen.Media.Data.Media where

import Data.Maybe               (Maybe(..))

-- | Media represents a basic media datatype
--   used througout the library.
--   Media is polymorphic over r where r is
--   any other record field 
newtype Media r = Media 
  { src       :: String
  , thumbnail :: Maybe String 
  | r 
  }

type MediaArray r = Array (Media r)
