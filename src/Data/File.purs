module Halogen.Media.Data.File where

import Web.File.File as File
import Data.UUID (UUID)
import Data.Maybe (Maybe)

type UploadStatus
  = Boolean

-- | Extended file contains a unique UUID
-- for identification and a (Maybe String)
-- that represents the thumbnail of the file
data ExtendedFile
  = ExtendedFile File.File UUID (Maybe String)

-- | Data type used internally by Component.Upload
--   Adds a UploadStatus type for upload status
data UploadFile
  = UploadFile ExtendedFile UploadStatus

type ExtendedFileArray
  = Array ExtendedFile

type UploadFileArray
  = Array UploadFile
