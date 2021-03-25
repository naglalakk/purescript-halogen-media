module Halogen.Media.Component.Modal where

import Prelude

import Data.Eq (class EqRecord)
import Data.Maybe (Maybe(..))
import Data.Show (class ShowRecordFields)
import Data.Symbol (SProxy(..))
import Data.UUID (UUID)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.Media.Component.Browser as Browser
import Halogen.Media.Component.CSS.Modal as ModalStyle
import Halogen.Media.Component.HTML.Utils (css)
import Halogen.Media.Data.Config.Upload (UploadConfig)
import Halogen.Media.Data.Media (Media, MediaArray)
import Prim.RowList as RL

-- | A modal for Component.Browser 
type ChildSlots r
  = ( browser :: H.Slot (Browser.Query r) (Browser.Output r) Unit
    )

data Query r a
  = SetUploadStatus UUID Boolean a
  | UpdateSelectedMedia (Media r) a

data Action r
  = Receive (Input r)
  | CloseModal
  | HandleBrowserAction (Browser.Output r)

type Input r
  = { isActive :: Boolean
    , media :: MediaArray r
    , uploadConfig :: Maybe UploadConfig
    }

type State r
  = { isActive :: Boolean
    , media :: MediaArray r
    , currentTab :: Maybe Browser.Tab
    , uploadConfig :: Maybe UploadConfig
    }

component ::
  forall m r l.
  RL.RowToList ( src :: String, thumbnail :: Maybe String | r ) l =>
  EqRecord l ( src :: String, thumbnail :: Maybe String | r ) =>
  ShowRecordFields l ( src :: String, thumbnail :: Maybe String | r ) =>
  MonadEffect m =>
  MonadAff m =>
  H.Component HH.HTML (Query r) (Input r) (Browser.Output r) m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval
          $ H.defaultEval
              { handleAction = handleAction
              , handleQuery = handleQuery
              , receive = Just <<< Receive
              }
    }
  where
  initialState :: (Input r) -> (State r)
  initialState inp =
    { isActive: inp.isActive
    , media: inp.media
    , currentTab: Just Browser.DisplayTab
    , uploadConfig: inp.uploadConfig
    }

  handleAction ::
    (Action r) ->
    H.HalogenM (State r) (Action r) (ChildSlots r) (Browser.Output r) m Unit
  handleAction = case _ of
    CloseModal -> do
      state <- H.get
      case state.isActive of
        false -> pure unit
        true ->
          H.modify_
            _
              { isActive = false
              , currentTab = Just Browser.DisplayTab
              }
      H.raise Browser.Closed
    HandleBrowserAction action -> case action of
      Browser.TabSwitch tab -> do
        H.modify_ _ { currentTab = Just tab }
        H.raise action
      Browser.InsertedMedia media -> do
        handleAction $ CloseModal
        H.raise action
      _ -> H.raise action
    Receive inp -> do
      H.modify_
        _
          { isActive = inp.isActive
          , media = inp.media
          }

  handleQuery ::
    forall a.
    Query r a ->
    H.HalogenM (State r) (Action r) (ChildSlots r) (Browser.Output r) m (Maybe a)
  handleQuery = case _ of
    SetUploadStatus uuid status a -> do
      _ <- H.query (SProxy :: SProxy "browser") unit (H.tell (Browser.SetUploadStatus uuid status))
      pure $ Just a
    UpdateSelectedMedia media a -> do
      _ <- H.query (SProxy :: SProxy "browser") unit (H.tell (Browser.UpdateSelectedMedia media))
      pure $ Just a

  render :: (State r) -> H.ComponentHTML (Action r) (ChildSlots r) m
  render state =
    HH.div
      [ css "modal-layer"
      , HCSS.style $ ModalStyle.modalLayer state.isActive
      ]
      [ ModalStyle.stylesheet
      , HCSS.stylesheet do
          ModalStyle.modalIsActive state.isActive
      , HH.div
          [ css "modal-container" ]
          [ HH.div
              [ css "modal-header"
              , HE.onClick \_ -> Just CloseModal
              ]
              [ HH.text "X" ]
          , HH.div
              [ css "modal-body" ]
              [ HH.slot
                  (SProxy :: _ "browser")
                  unit
                  Browser.component
                  { media: state.media
                  , selectedTab: state.currentTab
                  , uploadConfig: state.uploadConfig
                  }
                  (Just <<< HandleBrowserAction)
              ]
          ]
      ]
