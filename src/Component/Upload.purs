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
import Data.UUID                            (UUID(..)
                                            ,genUUID)
import Foreign                              (Foreign
                                            ,readString)
import Halogen                              as H
import Halogen.Aff                          as HA
import Halogen.HTML                         as HH
import Halogen.HTML.CSS                     as HCSS
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

import Halogen.Media.Data.File              (ExtendedFile(..)
                                            ,ExtendedFileArray)
import Halogen.Media.Component.HTML.Utils   (css)
import Halogen.Media.Component.CSS.Upload   as UploadStyle
import Halogen.Media.Data.File              (ExtendedFile(..)
                                            ,ExtendedFileArray
                                            ,UploadFile(..)
                                            ,UploadFileArray)
import Halogen.Media.Utils                  (fileListToFiles)


type State =
  { files :: UploadFileArray
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

type UploadStatus =
  { uuid :: UUID
  , status :: Boolean
  }
data Query a = SetUploadStatus UUID Boolean a

derive instance genericOutput :: Generic Output _

instance showOutput :: Show Output where
  show (UploadFiles  fl) =
    show $
      map (\(ExtendedFile f uuid thumb) -> File.name f) fl
  show (DroppedFiles fl) =
    show $
      map (\(ExtendedFile f uuid thumb) -> File.name f) fl


-- Util function for updating
-- the thumbnail of a UploadFile
setThumb :: String
         -> UploadFile
         -> UploadFile
setThumb thumb (UploadFile (ExtendedFile f u t) status)
  = UploadFile (ExtendedFile f u (Just thumb)) status

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
    , handleQuery  = handleQuery
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

  handleQuery :: forall a
               . Query a 
              -> H.HalogenM State Action ChildSlots Output m (Maybe a)
  handleQuery = case _ of
    (SetUploadStatus uuid status next) -> do
      state <- H.get
      let 
        newArr = map (\(UploadFile (ExtendedFile f u t) s) -> do
          case u == uuid of
            true -> UploadFile (ExtendedFile f u t) status
            false ->   UploadFile (ExtendedFile f u t) s) state.files
      H.modify_ _ { files = newArr }
      pure $ Just next

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
          let index = findIndex (\(UploadFile (ExtendedFile f u t) status) -> u == uuid) state.files
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

      let 
        uploadFiles = map (\(ExtendedFile file uuid thumb) -> UploadFile (ExtendedFile file uuid thumb) false) extendedFiles
        allFiles = state.files <> uploadFiles

      -- Get thumbnails for files
      H.modify_ _ { files = allFiles }

      -- Raise only the files
      -- being uploaded
      H.raise $ DroppedFiles extendedFiles

    SubmitFiles -> do
      state <- H.get
      let uploadFiles = map (\(UploadFile (ExtendedFile file uuid thumb) status) -> ExtendedFile file uuid thumb) state.files
      H.raise $ UploadFiles uploadFiles

  renderFile (UploadFile (ExtendedFile file uuid thumb) status) =
    HH.div
      [ css "upload-file" 
      ]
      [ HH.div
        [ css "upload-thumbnail" 
        ]
        [ HH.img
          [ HP.src $ fromMaybe "" thumb ]
        ]
      , HH.ul
        [ css "upload-info" 
        ]
        [ HH.li
          [ css "upload-title" ]
          [ HH.b
            []
            [ HH.text "Name: "]
          , HH.text $ File.name file 
          ]
        , HH.li
          [ css "upload-size" ]
          [ HH.b
            []
            [ HH.text "Filesize: "]
          , HH.text $ show $ File.size file ]
        , HH.li
          [ css $ "upload-status upload-" <> show status ]
          [ HH.div
            []
            [ HH.b
              []
              [ HH.text "Status: " ]
            , case status of 
              true -> HH.text "Completed"
              false -> HH.text "Pending"
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
        [ css "dropbox"
        , HE.onDragOver $ \e -> Just $ PreventDefault $ DE.toEvent e
        , HE.onDrop $ \e -> Just $ SetFiles e
        ]
        [ if (length state.files <= 0)
            then HH.text "Drop files here"
            else HH.text ""
        , HH.div
          [ css "uploads" 
          ]
          (state.files <#> renderFile)
        ]
      ]
