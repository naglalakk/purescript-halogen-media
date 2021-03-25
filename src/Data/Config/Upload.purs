module Halogen.Media.Data.Config.Upload where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)

newtype UploadConfig = UploadConfig 
  { maxUploads :: Int
  , defaultLabel :: String
  , pendingLabel :: String
  , completedLabel :: String
  , nameLabel :: String
  , fileSizeLabel :: String
  , statusLabel :: String
  , uploadThumbnail :: Maybe String
  }

derive instance newtypeUploadConfig :: Newtype UploadConfig _
derive instance genericUploadConfig :: Generic UploadConfig _
derive instance eqUploadConfig :: Eq UploadConfig


defaultUploadConfig :: UploadConfig
defaultUploadConfig = UploadConfig
  { maxUploads: 1000
  , defaultLabel: "Drop files here"
  , pendingLabel: "Pending"
  , completedLabel: "Completed"
  , nameLabel: "Name"
  , fileSizeLabel: "Filesize"
  , statusLabel: "Status"
  , uploadThumbnail: Nothing
  }
