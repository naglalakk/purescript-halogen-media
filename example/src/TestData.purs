module Example.TestData where

import Prelude
import Data.Array                           (replicate)
import Data.Maybe                           (Maybe(..))

import Halogen.Media.Data.Image             (Image(..), ImageArray)
import Halogen.Media.Data.Video             (Video(..), VideoArray)
import Halogen.Media.Data.Base              (Media(..), MediaArray)

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

-- | Generates n amount of videos 
medias :: Int -> MediaArray
medias n = mediaImages <> mediaVideos
  where
    mediaImages = map (\x -> MediaImage x) $ images amount
    mediaVideos = map (\x -> MediaVideo x) $ videos amount
    amount = n / 2
