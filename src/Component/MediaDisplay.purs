module Halogen.Media.Component.MediaDisplay where

import Effect.Class.Console                 (logShow)
import Prelude
import CSS                                  as CSS
import Data.Array                           (filter
                                            ,updateAt
                                            ,elemIndex)
import Data.Const                           (Const)
import Data.Maybe                           (Maybe(..)
                                            ,fromMaybe)
import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Show                (genericShow)
import Data.Traversable                     (traverse)
import Data.UUID                            (UUID(..)
                                            ,genUUID)
import Effect.Class                         (class MonadEffect)
import Halogen                              as H
import Halogen.HTML.CSS                     as HCSS
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties              as HP

import Halogen.Media.Data.Base              (MediaArray
                                            ,Media(..))
import Halogen.Media.Data.Image             (Image(..))
import Halogen.Media.Data.Video             (Video(..))
import Halogen.Media.Component.HTML.Utils   (css)

data UIMedia = UIMedia Media Boolean (Maybe UUID)

derive instance eqUIMedia :: Eq UIMedia
derive instance ordUIMedia :: Ord UIMedia

type State =
  { media :: Array UIMedia
  }

type Input =
  { media :: MediaArray
  }

type ChildSlots = ()
type Query = Const Void

data Action
  = Initialize
  | ClickMedia UIMedia
  | Receive Input

data Output
  = ClickedMedia Media

derive instance genericOutput :: Generic Output _
derive instance eqOutput :: Eq Output

instance showOutput :: Show Output where
  show = genericShow

component :: forall m
           . MonadEffect m
          => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval H.defaultEval
    { initialize = Just Initialize
    , handleAction = handleAction
    , receive = Just <<< Receive
    }
  }
  where
  initialState :: Input -> State
  initialState { media } =
    { media: map (\x -> (UIMedia x false Nothing)) media
    }

  handleAction = case _ of
    Initialize -> do
      state <- H.get
      newMedias <- traverse (\(UIMedia media selected uuid) -> do
        uuidNew <- H.liftEffect $ genUUID
        pure $ UIMedia media selected (Just uuidNew)) state.media
      H.modify_ _ { media = newMedias }
    (ClickMedia (UIMedia media selected uuid)) -> do
      state <- H.get
      let 
        sel = not selected
        newMedia = UIMedia media sel uuid
        elemIx = elemIndex (UIMedia media selected uuid) state.media
      case elemIx of
        Just ix -> do
          let arr = updateAt ix newMedia state.media
          case arr of 
            Just nm -> H.modify_ _ { media = nm }
            Nothing -> pure unit
        Nothing -> pure unit
      logShow "selected is"
      logShow selected
      H.raise $ ClickedMedia media
    Receive input -> do
      newMedias <- traverse (\media-> do
        uuidNew <- H.liftEffect $ genUUID
        pure $ UIMedia media false (Just uuidNew)) input.media
      H.modify_ _ { media = newMedias }

  renderMedia (UIMedia media selected uuid) =
    HH.a
      [ css "media-item"
      , case selected of
          true -> HCSS.style do (CSS.backgroundColor $ CSS.rgb 105 210 231)
          false -> HCSS.style do (CSS.backgroundColor $ CSS.rgb 255 255 255)
      , HE.onClick $ \_ -> Just $ ClickMedia $ UIMedia media selected uuid
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
