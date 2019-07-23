module Halogen.Media.Data.Image where

import Prelude
import Data.Maybe               (Maybe(..))
import Data.Argonaut.Encode     (class EncodeJson, encodeJson)
import Data.Argonaut.Decode     (class DecodeJson, decodeJson)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)

newtype Image = Image
  { id        :: Int
  , src       :: String
  , thumbnail :: Maybe String
  , name      :: String
  }

type ImageArray = Array Image

derive instance genericImage :: Generic Image _
derive instance eqImage :: Eq Image
derive instance ordImage :: Ord Image

instance showImage :: Show Image where
  show = genericShow

instance decodeJsonImage :: DecodeJson Image where
  decodeJson json = decodeJson json

instance encodeJsonImage :: EncodeJson Image where
  encodeJson (Image a) = encodeJson a
