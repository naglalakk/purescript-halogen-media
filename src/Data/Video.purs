module Halogen.Media.Data.Video where

import Prelude
import Data.Maybe               (Maybe(..))
import Data.Argonaut.Encode     (class EncodeJson, encodeJson)
import Data.Argonaut.Decode     (class DecodeJson, decodeJson)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)


newtype Video = Video
  { id  :: Int
  , src :: String
  , thumbnail :: Maybe String
  , title :: Maybe String
  }

type VideoArray = Array Video

derive instance genericVideo :: Generic Video _
derive instance eqVideo :: Eq Video
derive instance ordVideo :: Ord Video

instance showVideo :: Show Video where
  show = genericShow

instance decodeJsonVideo :: DecodeJson Video where
  decodeJson json = decodeJson json

instance encodeJsonVideo :: EncodeJson Video where
  encodeJson (Video a) = encodeJson a
