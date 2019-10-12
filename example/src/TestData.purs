module Example.TestData where

import Prelude
import Effect                               (Effect(..))
import Data.Array                           (fromFoldable
                                            ,replicate)
import Data.Foldable                        (class Foldable)
import Data.List.Lazy                       (toUnfoldable
                                            ,replicateM)
import Data.List                            (List(..))
import Data.Generic.Rep.Show                (genericShow)
import Data.Maybe                           (Maybe(..))
import Data.Traversable                     (traverse)
import Data.UUID                            (genUUID)

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
images :: Int -> Effect ImageArray
images n = do
  l <- replicateM n $ do
    randomS <- genUUID
    let 
      src = "https://picsum.photos/600/400?v=" <> (show randomS)
      thumbnail = Just $ "https://picsum.photos/150/150?v=" <> (show randomS)
    pure $ Image 
      { id: _id
      , src: src
      , thumbnail: thumbnail
      , name: name 
      }
  pure ((fromFoldable (toUnfoldable l :: List Image)) :: ImageArray)

  where
    _id = 0
    name = "testpic.jpg"


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
type Vid = ( id :: Int, title :: String)

-- | Generates n amount of images
--   wrapped in a Media type
medias :: Int -> Effect (MediaArray Img)
medias n = do
  imgs <- images n
  let mediaImages = map (\(Image x) -> Media x) imgs
  pure mediaImages
