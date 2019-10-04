module Halogen.Media.Data.File where

import Prelude
import Web.File.File        as File
import Data.UUID                            (UUID)
import Data.Maybe

-- | Extended file contains a unique UUID
-- for identification and a (Maybe String)
-- that represents the thumbnail of the file
data ExtendedFile
  = ExtendedFile File.File UUID (Maybe String)
