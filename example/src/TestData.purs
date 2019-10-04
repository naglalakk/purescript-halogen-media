module Example.TestData where

import Prelude
import Data.Array                           (replicate)
import Data.Generic.Rep.Show                (genericShow)
import Data.Maybe                           (Maybe(..))

import Halogen.Media.Data.Media             (Media(..)
                                            ,MediaArray)

newtype Image = Image
  { id        :: Int
  , src       :: String
  , thumbnail :: Maybe String
  , name      :: String
  }

type ImageArray = Array Image

newtype Video = Video
  { id  :: Int
  , src :: String
  , thumbnail :: Maybe String
  , title :: Maybe String
  }

type VideoArray = Array Video

-- | Generates n amount of fake images
--   src and thumbnail are set to dynamic urls
--   that should load different images for each
--   seperate object
images :: Int -> ImageArray
images n 
  = replicate n 
  $ Image 
    { id: _id
    , src: src
    , thumbnail: thumbnail
    , name: name 
    }
  where
    _id = 0
    name = "testpic.jpg"
    src = "https://picsum.photos/600/400/"
    thumbnail = Just "https://picsum.photos/150/150/"


-- | Generates n amount of fake videos
-- 
videos :: Int -> VideoArray
videos n
  = replicate n 
  $ Video 
    { id: _id
    , src: src
    , thumbnail: thumbnail
    , title: title
    }
  where
    _id = 0
    title = Just "Titanic - Leo Scene"
    src = "https://picsum.photos/600/400/"
    thumbnail = Just "https://picsum.photos/150/150/"

type Img = ( id :: Int, name :: String )

-- | Generates n amount of videos 
medias :: Int -> MediaArray Img
medias n = mediaImages
  where
    mediaImages = map (\(Image x) -> Media x) $ images n
