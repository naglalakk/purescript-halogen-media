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

import Halogen.Media.Data.Base              (Media(..)
                                            ,MediaArray)
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

type State =
  { media:: MediaArray
  , selectedTab :: Tab
  }

type Input =
  { media :: MediaArray
  , selectedTab :: Maybe Tab
  }

data Output
  = Clicked Media
  | Upload  (Array Upload.ExtendedFile)
  | Dropped (Array Upload.ExtendedFile)
  | TabSwitch Tab

type Query = Const Void

data Action
  = MDOutput MediaDisplay.Output
  | ULOutput Upload.Output
  | SwitchTab Tab
  | Receive Input

type ChildSlots = (
  mediaDisplay :: H.Slot Query MediaDisplay.Output Unit,
  upload :: H.Slot Query Upload.Output Unit
)

derive instance genericOutput :: Generic Output _
-- derive instance eqOutput :: Eq Output

instance showOutput :: Show Output where
  show (Clicked media) = show media
  show (Dropped files) =
    show $
      map (\(Upload.ExtendedFile f uuid thumb) -> File.name f) files
  show (Upload  files) =
    show $
      map (\(Upload.ExtendedFile f uuid thumb) -> File.name f) files
  show (TabSwitch tab) = show tab

component :: forall m
           . MonadEffect m
          => MonadAff m
          => H.Component HH.HTML Query Input Output m
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
  initialState :: Input -> State
  initialState input =
    { media: input.media
    , selectedTab: fromMaybe DisplayTab input.selectedTab
    }

  handleAction = case _ of
    (MDOutput (MediaDisplay.ClickedMedia media)) ->
      H.raise $ Clicked media
    (ULOutput (Upload.UploadFiles files)) ->
      H.raise $ Upload files
    (ULOutput (Upload.DroppedFiles files)) ->
      H.raise $ Dropped files
    SwitchTab tab -> do
      H.modify_ _ { selectedTab = tab }
      H.raise $ TabSwitch tab
    Receive inp -> do
      H.put inp { selectedTab = fromMaybe DisplayTab inp.selectedTab }

  render :: State -> H.ComponentHTML Action ChildSlots m
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
