module Halogen.Media.Data.Media where

import Data.UUID                            (UUID(..))
import Prelude
import Data.Maybe               (Maybe(..))
import Data.Eq                  (class EqRecord)
import Data.Show                (class ShowRecordFields)
import Prim.RowList             as RL

-- | Media represents a basic media datatype
--   used througout the library.
--   Media is polymorphic over r where r is
--   any other record field 
newtype Media r = Media 
  { src       :: String
  , thumbnail :: Maybe String 
  | r
  }

derive newtype instance eqMedia :: (RL.RowToList (src :: String, thumbnail :: Maybe String | row) list, EqRecord list ( src :: String, thumbnail :: Maybe String | row)) => Eq (Media row)

derive newtype instance showMedia :: (RL.RowToList (src :: String, thumbnail :: Maybe String | row) list, ShowRecordFields list ( src :: String, thumbnail :: Maybe String | row)) => Show (Media row)

-- | UIMedia is used internally by MediaDisplay 
--   component to keep track of clicked medias
data UIMedia r = UIMedia (Media r) Boolean (Maybe UUID)

derive instance eqUIMedia :: (RL.RowToList (src :: String, thumbnail :: Maybe String | row) list, EqRecord list ( src :: String, thumbnail :: Maybe String | row)) => Eq (UIMedia row)

type MediaArray r = Array (Media r)
