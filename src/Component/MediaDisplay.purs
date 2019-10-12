module Halogen.Media.Component.MediaDisplay where

import Effect.Class.Console                 (logShow)
import Prelude
import CSS                                  as CSS
import Data.Array                           (filter
                                            ,updateAt
                                            ,findIndex
                                            ,elemIndex)
import Data.Const                           (Const)
import Data.Maybe                           (Maybe(..)
                                            ,fromMaybe)
import Data.Eq                              (class EqRecord)
import Data.Show                            (class ShowRecordFields)
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
import Prim.RowList                         as RL

import Halogen.Media.Data.Media             (MediaArray
                                            ,Media(..)
                                            ,UIMedia(..))
import Halogen.Media.Component.HTML.Utils   (css)

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
  = ClickedMedia (MediaArray r)

{--
derive instance genericOutput :: Generic Output _
derive instance eqOutput :: Eq Output
--}

{--
instance showOutput :: Show Output where
  show = genericShow
--}

component :: forall m r l
           . RL.RowToList (src :: String, thumbnail :: Maybe String | r) l
          => EqRecord l ( src :: String, thumbnail :: Maybe String | r)
          => ShowRecordFields l ( src :: String, thumbnail :: Maybe String | r)
          => MonadEffect m 
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
    { media: map (\x -> ((UIMedia x) false Nothing)) media
    }

  handleAction = case _ of
    Initialize -> do
      state <- H.get
      newMedias <- traverse (\(UIMedia media selected uuid) -> do
        uuidNew <- H.liftEffect $ genUUID
        pure $ UIMedia media selected (Just uuidNew)) state.media
      H.modify_ _ { media = newMedias }
    (ClickMedia (UIMedia (Media media) selected uuid)) -> do
      state <- H.get
      let 
        sel = not selected
        newMedia = UIMedia (Media media) sel uuid
        elemIx = elemIndex (UIMedia (Media media) selected uuid) state.media
      case elemIx of
        Just ix -> do
          let arr = updateAt ix newMedia state.media
          logShow ix
          case arr of 
            Just nm -> do
              let
                allClicked = filter (\(UIMedia (Media media) s u) -> s == true) nm
                allClickedMedia = map (\(UIMedia (Media media) s u) -> (Media media)) allClicked
              H.modify_ _ { media = nm }
              H.raise $ ClickedMedia allClickedMedia
            Nothing -> pure unit
        Nothing -> pure unit

    Receive input -> do
      newMedias <- traverse (\media-> do
        uuidNew <- H.liftEffect $ genUUID
        pure $ UIMedia media false (Just uuidNew)) input.media
      H.modify_ _ { media = newMedias }

  renderMedia (UIMedia (Media media) selected uuid) =
    HH.a
      [ css $ "media-item selected-" <> show selected
      , HE.onClick $ \_ -> Just $ ClickMedia $ UIMedia (Media media) selected uuid
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
