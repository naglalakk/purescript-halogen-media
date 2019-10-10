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

import Halogen.Media.Data.Media             (MediaArray
                                            ,Media(..))
import Halogen.Media.Component.HTML.Utils   (css)

data UIMedia r = UIMedia (Media r) Boolean (Maybe UUID)

derive instance eqUIMedia :: Eq UIMedia
derive instance ordUIMedia :: Ord UIMedia

type State r =
  { media :: Array (UIMedia r)
  }

type Input r =
  { media :: MediaArray r
  }

type ChildSlots = ()
type Query = Const Void

data Action r
  = Initialize
  | ClickMedia (UIMedia r)
  | Receive (Input r)

data Output r
  = ClickedMedia (Media r)

derive instance genericOutput :: Generic Output _
derive instance eqOutput :: Eq Output

instance showOutput :: Show Output where
  show = genericShow

component :: forall m r
           . MonadEffect m 
           => H.Component HH.HTML Query (Input r) (Output r) m
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
  initialState :: Input r -> State r
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
