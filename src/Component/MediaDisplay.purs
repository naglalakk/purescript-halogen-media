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

import Halogen.Media.Data.Media             (MediaArray
                                            ,Media(..))
import Halogen.Media.Component.HTML.Utils   (css)

type State r =
  { media :: MediaArray r
  }

type Input r =
  { media :: MediaArray r
  }

type ChildSlots = ()
type Query = Const Void

data Action r
  = ClickMedia (Media r)
  | Receive (Input r)

data Output r
  = ClickedMedia (Media r)

component :: forall m r
           . H.Component HH.HTML Query (Input r) (Output r) m
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
  initialState :: Input r -> State r
  initialState { media } =
    { media: media
    }

  handleAction = case _ of
    (ClickMedia media) -> H.raise $ ClickedMedia media
    Receive input -> H.put input

  renderMedia (Media media) =
    HH.a
      [ css "media-item"
      , HE.onClick $ \_ -> Just $ ClickMedia (Media media)
      ]
      [ HH.div
        [ css "thumbnail" ]
        [ HH.img
          [ HP.src media.src ]
        ]
      ]

  render :: (State r) -> H.ComponentHTML (Action r) ChildSlots m
  render state =
    HH.div
      [ css "media" ]
      (state.media <#> renderMedia)
