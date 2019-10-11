module Halogen.Media.Component.Browser where

import Prelude
import Data.Array                           ((..))
import Effect.Class.Console                 (logShow)
import Effect.Aff.Class                     (class MonadAff)
import Effect.Class                         (class MonadEffect)
import Data.Const                           (Const)
import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Show                (genericShow)
import Data.Maybe                           (Maybe(..), fromMaybe)
import Data.Symbol                          (SProxy(..))
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties              as HP
import Web.File.FileList                    as FL
import Web.File.File                        as File
import Prim.RowList                         as RL
import Data.Eq                              (class EqRecord)
import Data.Show                            (class ShowRecordFields)

import Halogen.Media.Data.Media             (Media(..)
                                            ,MediaArray)
import Halogen.Media.Data.File              (ExtendedFile(..)
                                            ,ExtendedFileArray)
import Halogen.Media.Component.MediaDisplay as MediaDisplay
import Halogen.Media.Component.Upload       as Upload
import Halogen.Media.Component.HTML.Utils   (css)
import Halogen.Media.Utils                  (fileListToFiles)

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
  , selectedTab :: Tab
  }

type Input r =
  { media :: MediaArray r
  , selectedTab :: Maybe Tab
  }

data Output r
  = Clicked (MediaArray r)
  | Upload  ExtendedFileArray
  | Dropped ExtendedFileArray
  | TabSwitch Tab

type Query = Const Void

data Action r
  = MDOutput (MediaDisplay.Output r)
  | ULOutput Upload.Output
  | SwitchTab Tab
  | Receive (Input r)

type ChildSlots r = (
  mediaDisplay :: H.Slot Query (MediaDisplay.Output r) Unit,
  upload :: H.Slot Query Upload.Output Unit
)

-- derive instance genericOutput :: Generic Output _
-- derive instance eqOutput :: Eq Output

{--
instance showOutput :: Show Output where
  show (Clicked media) = show media
  show (Dropped files) =
    show $
      map (\(ExtendedFile f uuid thumb) -> File.name f) files
  show (Upload  files) =
    show $
      map (\(ExtendedFile f uuid thumb) -> File.name f) files
  show (TabSwitch tab) = show tab
--}

component :: forall m r l
           . RL.RowToList (src :: String, thumbnail :: Maybe String | r) l
          => EqRecord l ( src :: String, thumbnail :: Maybe String | r)
          => ShowRecordFields l ( src :: String, thumbnail :: Maybe String | r)
          => MonadEffect m
          => MonadAff m
          => H.Component HH.HTML Query (Input r) (Output r) m
component =
  H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , receive =  Just <<< Receive
    }
  }
  where
  initialState :: Input r -> State r
  initialState input =
    { media: input.media
    , selectedTab: fromMaybe DisplayTab input.selectedTab
    }

  handleAction = case _ of
    (MDOutput (MediaDisplay.ClickedMedia media)) -> do
      H.raise $ Clicked media
    (ULOutput (Upload.UploadFiles files)) ->
      H.raise $ Upload files
    (ULOutput (Upload.DroppedFiles files)) ->
      H.raise $ Dropped files
    SwitchTab tab -> do
      H.modify_ _ { selectedTab = tab }
      H.raise $ TabSwitch tab
    Receive inp -> H.put inp { selectedTab = fromMaybe DisplayTab inp.selectedTab }

  render :: (State r) -> H.ComponentHTML (Action r) (ChildSlots r) m
  render state =
    HH.div
      [ css "media-browser" ]
      [ HH.div
        [ css "tabs" ]
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
