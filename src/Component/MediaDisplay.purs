module Halogen.Media.Component.MediaDisplay where

import Prelude
import CSS                                      as CSS
import Data.Array                               (filter
                                                ,updateAt
                                                ,findIndex
                                                ,elemIndex)
import Data.Const                               (Const)
import Data.Maybe                               (Maybe(..)
                                                ,fromMaybe)
import Data.Eq                                  (class EqRecord)
import Data.Show                                (class ShowRecordFields)
import Data.Generic.Rep                         (class Generic)
import Data.Generic.Rep.Show                    (genericShow)
import Data.Traversable                         (traverse)
import Data.UUID                                (UUID(..)
                                                ,genUUID)
import Effect.Class                             (class MonadEffect)
import Effect.Aff.Class                         (class MonadAff)
import Effect.Class.Console                     (logShow)
import Halogen                                  as H
import Halogen.HTML.CSS                         as HCSS
import Halogen.HTML                             as HH
import Halogen.HTML.Events                      as HE
import Halogen.HTML.Properties                  as HP
import Halogen.Query.EventSource                as ES
import Prim.RowList                             as RL
import Web.Event.CustomEvent                    as CEV
import Web.Event.Event                          as EV
import Web.HTML                                 (HTMLDocument)
import Web.HTML (window)                        as Web
import Web.HTML.HTMLDocument                    as HTMLDocument
import Web.HTML.HTMLElement                     as HTMLElement
import Web.DOM.Element                          as Element
import Web.HTML.Window                          as Web
import Web.TouchEvent.EventTypes                as TET
import Web.TouchEvent.Touch                     as Touch
import Web.TouchEvent.TouchEvent                as TE
import Web.TouchEvent.TouchList                 as TouchList
import Web.UIEvent.KeyboardEvent                (KeyboardEvent)
import Web.UIEvent.KeyboardEvent                as KE
import Web.UIEvent.KeyboardEvent.EventTypes     as KET
import Web.UIEvent.WheelEvent                   as WE
import Web.UIEvent.WheelEvent.EventTypes        as WET

import Halogen.Media.Component.HTML.Utils       (css)
import Halogen.Media.Component.CSS.MediaDisplay as MediaDisplayStyle
import Halogen.Media.Data.Media                 (MediaArray
                                                ,Media(..)
                                                ,UIMedia(..))

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
  | RemoveMedia (UIMedia r)
  | HandleWheel WE.WheelEvent
  | Receive (Input r)

data Output r
  = ClickedMedia (MediaArray r)
  | RemovedMedia (Media r)
  | ScrollIsAtBottom

component :: forall m r l
           . RL.RowToList (src :: String, thumbnail :: Maybe String | r) l
          => EqRecord l ( src :: String, thumbnail :: Maybe String | r)
          => ShowRecordFields l ( src :: String, thumbnail :: Maybe String | r)
          => MonadAff m
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

      element <- H.getHTMLElementRef 
              $  H.RefLabel "media-container"
      case element of
        Just elem -> do
          -- subscribe
          H.subscribe' \sid ->
            ES.eventListenerEventSource
            WET.wheel
            (HTMLElement.toEventTarget elem)
            (map HandleWheel <<< WE.fromEvent)
        Nothing -> do
          pure unit
      H.modify_ _ { media = newMedias }

    HandleWheel ev -> do
      ref <- H.getHTMLElementRef 
          $ H.RefLabel "media-container"
      case ref of
        Just element -> do
          let domElement = HTMLElement.toElement element
          offsetHeight <- H.liftEffect $ HTMLElement.offsetHeight element
          scrollHeight <- H.liftEffect $ Element.scrollHeight domElement
          scrollTop <- H.liftEffect $ Element.scrollTop domElement

          let contentHeight = scrollHeight - offsetHeight
          case contentHeight <= scrollTop of
            true -> H.raise ScrollIsAtBottom
            false -> pure unit
        Nothing -> do
          pure unit

    (ClickMedia (UIMedia (Media media) selected uuid)) -> do
      state <- H.get
      let 
        sel = not selected
        newMedia = UIMedia (Media media) sel uuid
        elemIx = elemIndex (UIMedia (Media media) selected uuid) state.media
      case elemIx of
        Just ix -> do
          let arr = updateAt ix newMedia state.media
          case arr of 
            Just nm -> do
              let
                allClicked = filter (\(UIMedia (Media media) s u) -> s == true) nm
                allClickedMedia = map (\(UIMedia (Media media) s u) -> (Media media)) allClicked
              H.modify_ _ { media = nm }
              H.raise $ ClickedMedia allClickedMedia
            Nothing -> pure unit
        Nothing -> pure unit

    (RemoveMedia (UIMedia (Media media) selected uuid)) -> do
      state <- H.get
      let 
        newMedia = filter (\(UIMedia (Media m) s u) -> u /= uuid) state.media
      H.modify_ _ { media = newMedia }
      H.raise $ RemovedMedia $ Media media

    Receive input -> do
      state <- H.get
      let 
        currMedia = map (\(UIMedia (Media media) s u) -> (Media media)) state.media
      case currMedia == input.media of
        true -> pure unit
        false -> do
          newMedias <- traverse (\media-> do
            uuidNew <- H.liftEffect $ genUUID
            pure $ UIMedia media false (Just uuidNew)) input.media
          H.modify_ _ { media = newMedias }

  renderMedia (UIMedia (Media media) selected uuid) =
    HH.a
      [ css $ "media-item selected-" <> show selected
      , HE.onClick \_ -> Just $ ClickMedia $ UIMedia (Media media) selected uuid
      , HCSS.style $ MediaDisplayStyle.selected selected
      ]
      [ HH.div
        [ css "thumbnail" 
        ]
        [ HH.div
          [ css "thumbnail-overlay" 
          ]
          [ HH.div
            [ css "thumbnail-remove" 
            , HE.onClick \_ -> Just $ RemoveMedia $ UIMedia (Media media) selected uuid
            ]
            [ HH.text "X"
            ]
          ]
        , case media.thumbnail of
          Just thumb -> HH.img [ HP.src thumb 
                               , css "thumbnail-image"
                               ]
          Nothing    -> HH.img [ HP.src media.src 
                               , css "thumbnail-image"
                               ]
        ]
      ]

  render :: (State r) -> H.ComponentHTML (Action r) ChildSlots m
  render state =
    HH.div
      []
      [ MediaDisplayStyle.stylesheet
      , HH.div
        [ css "media" 
        , HP.ref $ H.RefLabel "media-container" 
        ]
        (state.media <#> renderMedia)
      ]
