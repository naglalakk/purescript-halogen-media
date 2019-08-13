module Halogen.Media.Component.MediaDisplay where

import Prelude
import Data.Const                           (Const)
import Data.Maybe                           (Maybe(..)
                                            ,fromMaybe)
import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Show                (genericShow)
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties              as HP

import Halogen.Media.Data.Base              (MediaArray
                                            ,Media(..))
import Halogen.Media.Data.Image             (Image(..))
import Halogen.Media.Data.Video             (Video(..))
import Halogen.Media.Component.HTML.Utils   (css)

type State =
  { media :: MediaArray
  }

type Input =
  { media :: MediaArray
  }

type ChildSlots = ()
type Query = Const Void

data Action
  = ClickMedia Media
  | Receive Input

data Output
  = ClickedMedia Media

derive instance genericOutput :: Generic Output _
derive instance eqOutput :: Eq Output

instance showOutput :: Show Output where
  show = genericShow

component :: forall m. H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , receive = Just <<< Receive
    }
  }
  where
  initialState :: Input -> State
  initialState { media } =
    { media: media
    }

  handleAction = case _ of
    (ClickMedia media) -> H.raise $ ClickedMedia media
    Receive input -> H.put input

  renderMedia media =
    HH.div
      [ css "media-item"
      , HE.onClick $ \_ -> Just $ ClickMedia media
      ]
      [ case media of
        (MediaImage (Image img)) ->
          HH.div
            [ css "thumbnail" ]
            [ HH.img
              [ HP.src img.src ]
            ]
        (MediaVideo (Video video)) ->
          HH.div
            [ css "thumbnail" ]
            [ HH.img
              [ HP.src video.src ]
            ]
      ]

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ css "media" ]
      (state.media <#> renderMedia)
