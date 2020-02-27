module Halogen.Media.Component.Browser where

import Prelude

import Data.Array                           (snoc)
import Data.Const                           (Const)
import Data.Eq                              (class EqRecord)
import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Show                (genericShow)
import Data.Maybe                           (Maybe(..)
                                            ,fromMaybe)
import Data.Show                            (class ShowRecordFields)
import Data.Symbol                          (SProxy(..))
import Data.UUID                            (UUID(..))
import Effect.Aff.Class                     (class MonadAff)
import Effect.Class                         (class MonadEffect)
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.CSS                     (style)
import Halogen.HTML.Events                  as HE
import Prim.RowList                         as RL

import Halogen.Media.Component.CSS.Browser  as BrowserStyle
import Halogen.Media.Component.HTML.Utils   (css)
import Halogen.Media.Component.MediaDisplay as MediaDisplay
import Halogen.Media.Component.Upload       as Upload
import Halogen.Media.Data.File              (ExtendedFileArray)
import Halogen.Media.Data.Media             (Media, MediaArray)

data Tab
  = DisplayTab
  | UploadTab

derive instance genericTab :: Generic Tab _
derive instance eqTab :: Eq Tab
derive instance ordTab :: Ord Tab

instance showTab :: Show Tab where
  show = genericShow

type State r =
  { media :: MediaArray r
  , selectedMedia :: MediaArray r
  , selectedTab :: Tab
  }

type Input r =
  { media :: MediaArray r
  , selectedTab :: Maybe Tab
  }

data Output r
  = Clicked (MediaArray r)
  | Removed (Media r)
  | Upload  ExtendedFileArray
  | Dropped ExtendedFileArray
  | InsertedMedia (MediaArray r)
  | TabSwitch Tab

data Query r a 
  = SetUploadStatus UUID Boolean a
  | UpdateSelectedMedia (Media r) a

data Action r
  = MDOutput (MediaDisplay.Output r)
  | ULOutput Upload.Output
  | SwitchTab Tab
  | InsertMedia
  | Receive (Input r)

type ChildSlots r = (
  mediaDisplay :: H.Slot MediaDisplay.Query (MediaDisplay.Output r) Unit,
  upload :: H.Slot Upload.Query Upload.Output Unit
)

component :: forall m r l
           . RL.RowToList (src :: String, thumbnail :: Maybe String | r) l
          => EqRecord l ( src :: String, thumbnail :: Maybe String | r)
          => ShowRecordFields l ( src :: String, thumbnail :: Maybe String | r)
          => MonadEffect m
          => MonadAff m
          => H.Component HH.HTML (Query r) (Input r) (Output r) m
component =
  H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , receive =  Just <<< Receive
    }
  }
  where
  initialState :: Input r -> State r
  initialState input =
    { media: input.media
    , selectedTab: fromMaybe DisplayTab input.selectedTab
    , selectedMedia: []
    }

  handleQuery :: forall a
               . Query r a
              -> H.HalogenM (State r) (Action r) (ChildSlots r) (Output r) m (Maybe a)
  handleQuery = case _ of
    SetUploadStatus uuid status a -> do
      _ <- H.query (SProxy :: SProxy "upload") unit (H.tell (Upload.SetUploadStatus uuid status))
      pure $ Just a
    UpdateSelectedMedia media a -> do
      state <- H.get
      H.modify_ _ 
        { selectedMedia = snoc state.selectedMedia media 
        }
      pure $ Just a

  handleAction = case _ of
    (MDOutput (MediaDisplay.ClickedMedia media)) -> do
      H.modify_ _ { selectedMedia = media }
      H.raise $ Clicked media
    (MDOutput (MediaDisplay.RemovedMedia media)) ->
      H.raise $ Removed media
    (ULOutput (Upload.UploadFiles files)) ->
      H.raise $ Upload files
    (ULOutput (Upload.DroppedFiles files)) ->
      H.raise $ Dropped files
    SwitchTab tab -> do
      H.modify_ _ { selectedTab = tab }
      H.raise $ TabSwitch tab
    InsertMedia -> do
      state <- H.get
      H.raise $ InsertedMedia state.selectedMedia
    Receive inp -> do
      H.modify_ _ { selectedTab = fromMaybe DisplayTab inp.selectedTab 
                  , media = inp.media
                  }

  render :: (State r) -> H.ComponentHTML (Action r) (ChildSlots r) m
  render state =
    HH.div
      [ css "media-browser" 
      ]
      [ BrowserStyle.stylesheet
      , HH.div
        [ css "tabs" 
        ]
        [ HH.div
          [ css "tab media-tab"
          , HE.onClick $ \e -> Just $ SwitchTab DisplayTab
          ]
          [ HH.text "Media" ]
        , HH.div
          [ css "tab upload-tab"
          , HE.onClick $ \e -> Just $ SwitchTab UploadTab
          ]
          [ HH.text "Upload" ]
        , HH.div
          [ css "tab insert-tab" 
          , HE.onClick $ \e -> Just InsertMedia
          ]
          [ HH.text "Insert" ]
        ]
      , case (state.selectedTab) of
          DisplayTab ->
            HH.slot
              (SProxy :: _ "mediaDisplay")
              unit
              MediaDisplay.component
              { media: state.media }
              (Just <<< MDOutput)
          UploadTab  ->
            HH.slot
              (SProxy :: _ "upload")
              unit
              Upload.component
              unit
              (Just <<< ULOutput)
      ]

