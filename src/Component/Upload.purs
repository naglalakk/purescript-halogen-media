module Halogen.Media.Component.Upload where

import Prelude

import CSS as CSS
import Control.Monad.Except (runExcept)
import Data.Array (length, findIndex, modifyAt)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (unwrap)
import Data.Traversable (traverse)
import Data.UUID (UUID, genUUID)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import FileSize (getHumanSize)
import Foreign (Foreign, readString)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.CSS as HCSS
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Media.Component.CSS.Upload as UploadStyle
import Halogen.Media.Component.HTML.Utils (css)
import Halogen.Media.Data.Config.Upload (UploadConfig(..), defaultUploadConfig)
import Halogen.Media.Data.File (ExtendedFile(..), ExtendedFileArray, UploadFile(..), UploadFileArray)
import Halogen.Media.Utils (fileListToFiles, verifyFiles)
import Halogen.Query.EventSource as HES
import Partial.Unsafe (unsafePartial)
import Web.Event.Event as EV
import Web.File.File (File)
import Web.File.File as File
import Web.File.FileReader as FileReader
import Web.HTML.Event.DataTransfer as DT
import Web.HTML.Event.DragEvent as DE
import Web.HTML.Event.EventTypes (load)

type State
  = { files :: UploadFileArray
    , reader :: Maybe FileReader.FileReader
    , config :: UploadConfig
    , acceptError :: Boolean
    }

data Output
  = DroppedFiles ExtendedFileArray
  | UploadFiles ExtendedFileArray

type Input = 
  { config :: Maybe UploadConfig
  }

data Action
  = Initialize
  | HandleFileUpload UUID (Effect Foreign)
  | ProcessFiles (Array File)
  | SetFilesMobile (Array File)
  | SetFiles DE.DragEvent
  | SubmitFiles
  | PreventDefault EV.Event

type ChildSlots
  = ()

data Query a
  = SetUploadStatus UUID Boolean a

derive instance genericOutput :: Generic Output _

instance showOutput :: Show Output where
  show (DroppedFiles fl) =
    show
      $ map (\(ExtendedFile f uuid thumb) -> File.name f) fl
  show (UploadFiles fl) =
    show
      $ map (\(ExtendedFile f uuid thumb) -> File.name f) fl

-- Util function for updating
-- the thumbnail of a UploadFile
setThumb ::
  String ->
  UploadFile ->
  UploadFile
setThumb thumb (UploadFile (ExtendedFile f u t) status) = UploadFile (ExtendedFile f u (Just thumb)) status

component ::
  forall m.
  MonadEffect m =>
  MonadAff m =>
  H.Component HH.HTML Query Input Output m
component =
  H.mkComponent
    { initialState: initialState
    , render
    , eval:
        H.mkEval
          H.defaultEval
            { handleAction = handleAction
            , handleQuery = handleQuery
            , initialize = Just Initialize
            }
    }
  where
  initialState :: Input -> State
  initialState inp =
    { files: []
    , reader: Nothing
    , config: fromMaybe defaultUploadConfig inp.config
    , acceptError: false
    }

  fileCallback :: EV.Event -> Maybe (Effect Foreign)
  fileCallback ev = do
    let
      eventTarget = EV.target ev
    case eventTarget of
      Just et -> do
        let
          fileReader = FileReader.fromEventTarget et
        case fileReader of
          Just r -> Just $ FileReader.result r
          Nothing -> Nothing
      Nothing -> Nothing

  fileEventSource ::
    FileReader.FileReader ->
    HES.EventSource m (Effect Foreign)
  fileEventSource fileReader =
    HES.eventListenerEventSource
      load
      (FileReader.toEventTarget fileReader)
      fileCallback

  handleQuery ::
    forall a.
    Query a ->
    H.HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    (SetUploadStatus uuid status next) -> do
      state <- H.get
      let
        newArr =
          map
            ( \(UploadFile (ExtendedFile f u t) s) -> do
                case u == uuid of
                  true -> UploadFile (ExtendedFile f u t) status
                  false -> UploadFile (ExtendedFile f u t) s
            )
            state.files
      H.modify_ _ { files = newArr }
      pure $ Just next

  handleAction :: Action -> H.HalogenM State Action ChildSlots Output m Unit
  handleAction = case _ of
    Initialize -> do
      pure unit
    HandleFileUpload uuid fr -> do
      state <- H.get
      src <- H.liftEffect fr
      let
        conf = unwrap state.config
        thumbnail = case conf.uploadThumbnail of
          Just thumb -> Right thumb
          Nothing    -> runExcept $ readString src
      case thumbnail of
        Right thumb -> do
          let
            index = findIndex (\(UploadFile (ExtendedFile f u t) status) -> u == uuid) state.files
          case index of
            Just ix -> do
              let
                newArr = modifyAt ix (setThumb thumb) state.files
              case newArr of
                Just arr -> H.modify_ _ { files = arr }
                Nothing -> pure unit
            Nothing -> pure unit
        Left err -> logShow err

    PreventDefault ev -> H.liftEffect $ EV.preventDefault ev

    ProcessFiles files -> do
      H.modify_ _ { acceptError = false }
      state <- H.get
      extendedFiles <-
        traverse
          ( \x -> do
              uuid <- H.liftEffect $ genUUID
              reader <- H.liftEffect $ FileReader.fileReader
              _ <- H.subscribe (HandleFileUpload uuid <$> (fileEventSource reader))
              let
                blob = File.toBlob x
              H.liftEffect $ FileReader.readAsDataURL blob reader
              pure $ ExtendedFile x uuid Nothing
          )
          files
      let
        uploadFiles = map (\(ExtendedFile file uuid thumb) -> UploadFile (ExtendedFile file uuid thumb) false) extendedFiles

        allFiles = state.files <> uploadFiles
      -- Get thumbnails for files
      H.modify_ _ { files = allFiles }
      -- Raise only the files
      -- being uploaded
      H.raise $ DroppedFiles extendedFiles

    SetFilesMobile files -> do
      state <- H.get
      let
        accept = (unwrap state.config).accept
      case verifyFiles accept files of 
        true -> handleAction $ ProcessFiles files
        false -> H.modify_ _ { acceptError = true }

    SetFiles ev -> do
      state <- H.get
      let
        event = DE.toEvent ev
        maxUploads = (unwrap state.config).maxUploads
        accept = (unwrap state.config).accept
        hasReachedLimit = length state.files == maxUploads
      H.liftEffect $ EV.preventDefault event
      if hasReachedLimit
        then pure unit
        else do
          let
            eventTarget =
              unsafePartial
                $ fromJust
                $ EV.currentTarget event

            droppedFiles =
              fileListToFiles
                $ DT.files $ DE.dataTransfer ev
          case verifyFiles accept droppedFiles of 
            true -> handleAction $ ProcessFiles droppedFiles
            false -> H.modify_ _ { acceptError = true }

    SubmitFiles -> do
      state <- H.get
      let
        uploadFiles = map (\(UploadFile (ExtendedFile file uuid thumb) status) -> ExtendedFile file uuid thumb) state.files
      H.raise $ UploadFiles uploadFiles

  renderFile (UploadConfig conf) (UploadFile (ExtendedFile file uuid thumb) status) =
    HH.div
      [ css "upload-file"
      ]
      [ HH.div
          [ css "upload-thumbnail"
          , HCSS.style do
              CSS.backgroundImage $ CSS.url (fromMaybe "" thumb)
          ] []
      , HH.ul
          [ css "upload-info"
          ]
          [ HH.li
              [ css "upload-title" ]
              [ HH.b
                  []
                  [ HH.text $ conf.nameLabel <> ": " ]
              , HH.text $ File.name file
              ]
          , HH.li
              [ css "upload-size" ]
              [ HH.b
                  []
                  [ HH.text $ conf.fileSizeLabel <> ": " ]
              , HH.text $ getHumanSize $ File.size file
              ]
          , HH.li
              [ css $ "upload-status upload-" <> show status ]
              [ HH.div
                  []
                  [ HH.b
                      []
                      [ HH.text $ conf.statusLabel <> ": " ]
                  , case status of
                      true -> HH.text conf.completedLabel
                      false -> HH.text conf.pendingLabel
                  ]
              ]
          ]
      ]

  render :: State -> H.ComponentHTML Action ChildSlots m
  render state =
    HH.div
      [ css "upload-container"
      ]
      [ UploadStyle.stylesheet
      , HH.div
        [ css "upload-mobile" ]
        [ HH.input
          [ css "upload-mobile" 
          , HP.type_ HP.InputFile
          , HE.onFileUpload $ \files -> Just $ SetFilesMobile files
          ]
        , case state.acceptError of
            true -> HH.p [] [ HH.text $ (unwrap state.config).acceptError ]
            false -> HH.div [] []
        ]
      , HH.div
          [ css "dropbox"
          , HE.onDragOver $ \e -> Just $ PreventDefault $ DE.toEvent e
          , HE.onDrop $ \e -> Just $ SetFiles e
          ]
          [ HH.label
            [ css "dropbox-label" ]
            [ if (length state.files <= 0) 
                then
                  if state.acceptError then
                    HH.text $ (unwrap state.config).acceptError
                  else
                    HH.text $ (unwrap state.config).defaultLabel
              else
                HH.text ""
            ]
          , HH.div
              [ css "uploads"
              ]
              (state.files <#> (renderFile state.config))
          ]
      ]
