module Halogen.Media.Component.Upload where

import Prelude
import Effect.Class                         (class MonadEffect)
import Effect.Class.Console                 (logShow)
import Data.Array                           ((..))
import Data.Const                           (Const)
import Data.Maybe                           (Maybe(..)
                                            ,fromJust
                                            ,fromMaybe)
import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Show                (genericShow)
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties              as HP
import Web.Event.Event                      (Event, preventDefault)
import Web.File.FileList                    as FL
import Web.File.File                        as File
import Web.HTML.Event.DragEvent             as DE
import Web.HTML.Event.DataTransfer          as DT

import Halogen.Media.Component.HTML.Utils   (css)
import Halogen.Media.Utils                  (fileListToFiles)

type State =
  { files :: Array File.File
  }

data Output 
  = UploadFiles (Array File.File)

type Input = Unit

data Action 
  = SetFiles DE.DragEvent
  | PreventDefault Event

type ChildSlots = ()
type Query = Const Void

derive instance genericOutput :: Generic Output _

instance showOutput :: Show Output where
  show (UploadFiles fl) = show $ map File.name fl

component :: forall m
           . MonadEffect m
          => H.Component HH.HTML Query Input Output m
component = 
  H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    }
  }
  where
  initialState :: State
  initialState =
    { files: []
    }

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    SetFiles ev -> do
      H.liftEffect $ preventDefault $ DE.toEvent ev
      let uFiles = fileListToFiles $ DT.files $ DE.dataTransfer ev
      state <- H.get 
      let allFiles = state.files <> uFiles
      H.modify_ _ { files = allFiles }
      H.raise $ UploadFiles allFiles
    PreventDefault ev ->
      H.liftEffect $ preventDefault ev

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state = 
    HH.div 
      [ css "upload-container" ]
      [ HH.div
        [ css "dropbox" 
        , HE.onDragOver $ \e -> Just $ PreventDefault $ DE.toEvent e
        , HE.onDrop $ \e -> Just $ SetFiles e 
        ]
        [ HH.text "Drop files here" 
        ]
      ]
