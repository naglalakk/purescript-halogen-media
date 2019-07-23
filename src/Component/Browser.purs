module Halogen.Media.Component.Browser where

import Prelude
import Data.Array                           ((..))
import Effect.Class.Console                 (logShow)
import Effect.Class                         (class MonadEffect)
import Data.Const                           (Const)
import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Show                (genericShow)
import Data.Maybe                           (Maybe(..))
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
import Halogen.Media.Utils                  (fileListToFiles)
                                          
data Tab
  = DisplayTab
  | UploadTab

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
  | Files   (Array File.File)

type Query = Const Void

data Action 
  = MDOutput MediaDisplay.Output
  | ULOutput Upload.Output

type ChildSlots = (
  mediaDisplay :: H.Slot Query MediaDisplay.Output Unit,
  upload :: H.Slot Query Upload.Output Unit
)

derive instance genericOutput :: Generic Output _
-- derive instance eqOutput :: Eq Output

instance showOutput :: Show Output where
  show (Clicked media) = show media
  show (Files   files) = show $ map File.name files

component :: forall m
           . MonadEffect m
          => H.Component HH.HTML Query Input Output m
component = 
  H.mkComponent
  { initialState: initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    }
  }
  where
  initialState :: Input -> State
  initialState input = 
    { media: input.media
    , selectedTab: case (input.selectedTab) of
      Just t -> t
      Nothing -> DisplayTab
    }

  handleAction = case _ of
    (MDOutput (MediaDisplay.ClickedMedia media)) -> 
      H.raise $ Clicked media
    (ULOutput (Upload.UploadFiles files)) ->
      H.raise $ Files files


  render :: State -> H.ComponentHTML Action ChildSlots m 
  render state =
    case (state.selectedTab) of
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
