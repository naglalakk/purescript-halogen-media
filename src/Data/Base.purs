module Halogen.Media.Data.Base where

import Prelude
import Data.Argonaut.Encode     (class EncodeJson, encodeJson)
import Data.Argonaut.Decode     (class DecodeJson, decodeJson)
import Data.Generic.Rep         (class Generic)
import Data.Generic.Rep.Show    (genericShow)

import Halogen.Media.Data.Image
import Halogen.Media.Data.Video

-- Media is a combined type of any media format
-- we want to be able to display | upload
data Media
  = MediaImage Image
  | MediaVideo Video

type MediaArray = Array Media

derive instance genericMedia :: Generic Media _
derive instance eqMedia :: Eq Media
derive instance ordMedia :: Ord Media

instance showMedia :: Show Media where
  show = genericShow

instance encodeJsonMedia :: EncodeJson Media where
  encodeJson (MediaImage img) = encodeJson img
  encodeJson (MediaVideo vid) = encodeJson vid