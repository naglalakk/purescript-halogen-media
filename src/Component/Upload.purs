module Halogen.Media.Component.Upload where

import Prelude
import Control.Monad.Except                 (runExcept)
import Control.Monad.State.Class            (class MonadState)
import Effect                               (Effect)
import Effect.Class                         (class MonadEffect)
import Effect.Class.Console                 (logShow)
import Effect.Aff                           (launchAff, runAff)
import Effect.Aff.Class                     (class MonadAff, liftAff)
import Data.Array                           ((..)
                                            ,filter
                                            ,length
                                            ,findIndex
                                            ,modifyAt
                                            ,(!!)
                                            ,updateAt)
import Data.Const                           (Const)
import Data.Either                          (Either(..))
import Data.Maybe                           (Maybe(..)
                                            ,fromJust
                                            ,fromMaybe)
import Data.Generic.Rep                     (class Generic)
import Data.Generic.Rep.Show                (genericShow)
import Data.Traversable                     (traverse)
import Foreign                              (Foreign
                                            ,readString)
import Halogen                              as H
import Halogen.Aff                          as HA
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties              as HP
import Halogen.Query.EventSource            as HES
import Partial.Unsafe                       (unsafePartial)
import Web.Event.Event                      as EV
import Web.Event.EventTarget                (EventTarget
                                            ,eventListener
                                            ,addEventListener)
import Web.HTML.Event.EventTypes            (load)
import Web.File.FileList                    as FL
import Web.File.File                        as File
import Web.File.FileReader                  as FileReader
import Web.HTML.Event.DragEvent             as DE
import Web.HTML.Event.DataTransfer          as DT

import Halogen.Media.Component.HTML.Utils   (css)
import Halogen.Media.Data.File              (ExtendedFile(..)
                                            ,ExtendedFileArray)
import Halogen.Media.Utils                  (fileListToFiles)
import Data.UUID                            (UUID(..)
                                            ,genUUID)


type State =
  { files  :: ExtendedFileArray
  , reader :: Maybe FileReader.FileReader
  }

data Output
  = DroppedFiles ExtendedFileArray
  | UploadFiles  ExtendedFileArray

type Input = Unit

data Action
  = Initialize
  | HandleFileUpload UUID (Effect Foreign)
  | SetFiles DE.DragEvent
  | SubmitFiles
  | PreventDefault EV.Event

type ChildSlots = ()
type Query = Const Void

derive instance genericOutput :: Generic Output _

instance showOutput :: Show Output where
  show (UploadFiles  fl) =
    show $
      map (\(ExtendedFile f uuid thumb) -> File.name f) fl
  show (DroppedFiles fl) =
    show $
      map (\(ExtendedFile f uuid thumb) -> File.name f) fl


-- Util function for updating
-- the thumbnail of a Extended file
setThumb :: String
         -> ExtendedFile
         -> ExtendedFile
setThumb thumb (ExtendedFile f u t)
  = ExtendedFile f u (Just thumb)

component :: forall m
           . MonadEffect m
          => MonadAff m
          => H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
  { initialState: const initialState
  , render
  , eval: H.mkEval H.defaultEval
    { handleAction = handleAction
    , initialize = Just Initialize
    }
  }
  where
  initialState :: State
  initialState =
    { files: []
    , reader: Nothing
    }

  fileCallback :: EV.Event -> Maybe (Effect Foreign)
  fileCallback ev = do
    let eventTarget = EV.target ev
    case eventTarget of
      Just et -> do
        let fileReader = FileReader.fromEventTarget et
        case fileReader of
          Just r  -> Just $ FileReader.result r
          Nothing -> Nothing
      Nothing -> Nothing

  fileEventSource :: FileReader.FileReader
                  -> HES.EventSource m (Effect Foreign)
  fileEventSource fileReader =
    HES.eventListenerEventSource
      load
      (FileReader.toEventTarget fileReader)
      fileCallback

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit

    HandleFileUpload uuid fr -> do
      src <- H.liftEffect fr
      let thumbnail = runExcept $ readString src
      case thumbnail of
        Right thumb -> do
          state <- H.get
          let index = findIndex (\(ExtendedFile f u t) -> u == uuid) state.files
          case index of
            Just ix -> do
              let newArr = modifyAt ix (setThumb thumb) state.files
              case newArr of
                Just arr -> H.modify_ _ { files = arr }
                Nothing  -> pure unit
            Nothing -> pure unit
        Left err -> logShow err

    PreventDefault ev ->
      H.liftEffect $ EV.preventDefault ev

    SetFiles ev -> do
      state <- H.get
      let
        event = DE.toEvent ev
        eventTarget = unsafePartial
                    $ fromJust
                    $ EV.currentTarget event
        droppedFiles = fileListToFiles
                     $ DT.files
                     $ DE.dataTransfer ev

      H.liftEffect $ EV.preventDefault event
      extendedFiles <- traverse (\x -> do
        uuid <- H.liftEffect $ genUUID
        reader <- H.liftEffect $ FileReader.fileReader
        _ <- H.subscribe (HandleFileUpload uuid <$> (fileEventSource reader))
        let blob = File.toBlob x
        H.liftEffect $ FileReader.readAsDataURL blob reader
        pure $ ExtendedFile x uuid Nothing
      ) droppedFiles

      let allFiles = state.files <> extendedFiles

      -- Get thumbnails for files
      H.modify_ _ { files = allFiles }

      -- Raise only the files
      -- being uploaded
      H.raise $ DroppedFiles extendedFiles

    SubmitFiles -> do
      state <- H.get
      H.raise $ UploadFiles state.files

  renderFile (ExtendedFile file uuid thumb) =
    HH.div
      [ css "upload-file" ]
      [ HH.div
        [ css "upload-thumbnail" ]
        [ HH.img
          [ HP.src $ fromMaybe "" thumb ]
        ]
      , HH.div
        [ css "upload-info" ]
        [ HH.div
          [ css "upload-title" ]
          [ HH.text $ File.name file ]
        , HH.div
          [ css "upload-size" ]
          [ HH.text $ show $ File.size file ]
        ]
      ]

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ css "upload-container" ]
      [ HH.div
        [ css "dropbox"
        , HE.onDragOver $ \e -> Just $ PreventDefault $ DE.toEvent e
        , HE.onDrop $ \e -> Just $ SetFiles e
        ]
        [ if (length state.files <= 0)
            then HH.text "Drop files here"
            else HH.text ""
        , HH.div
          [css "uploads" ]
          (state.files <#> renderFile)
        ]
      , HH.div
        [ css "upload-action" ]
        [ HH.div
          [ css "button"
          , HE.onClick $ \e -> Just SubmitFiles
          ]
          [ HH.text "Upload" ]
        ]
      ]
