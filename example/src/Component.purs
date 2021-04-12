module Example.Component where

import Prelude

import Data.Const (Const)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (logShow)
import Example.TestData (Img, medias)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Media.Component.Browser as Browser
import Halogen.Media.Component.Modal as Modal
import Halogen.Media.Data.Config.Upload (UploadConfig(..))
import Halogen.Media.Data.File (ExtendedFile(..))
import Halogen.Media.Data.Media (MediaArray, Media(..))

type State = 
  { isActive :: Boolean
  , media :: MediaArray Img
  }

data Action
  = Initialize
  | OpenModal
  | HandleBrowserAction (Browser.Output Img)

type Input = Unit

type Query = (Browser.Query Img)

type ChildSlots = (
  modal :: H.Slot (Modal.Query Img) (Browser.Output Img) Unit
)

component :: forall m
           . MonadEffect m
          => MonadAff m
          => H.Component HH.HTML Query Input Void m
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
    { isActive: false
    , media: []
    }

  handleAction = case _ of
    Initialize -> do
      images <- H.liftEffect $ medias 50
      H.modify_ _ { media = images }

    HandleBrowserAction (Browser.Clicked output) -> do
      logShow "Clicked images:"
      logShow output

    HandleBrowserAction Browser.Closed -> do
      logShow "You closed the media browser"
    
    HandleBrowserAction (Browser.Removed output) -> do
      logShow "You just removed a image"

    HandleBrowserAction (Browser.InsertedMedia media) -> do
      logShow "You inserted some media collected from the media items you clicked"
      logShow media

    HandleBrowserAction (Browser.Upload files) -> 
      logShow "Hey you just clicked the upload button, good job!"

    HandleBrowserAction (Browser.Dropped files) -> do
      _ <- traverse (\(ExtendedFile f uuid t) -> do
        H.query (SProxy :: SProxy "modal") unit (H.tell (Modal.SetUploadStatus uuid true))) files
      logShow "You dropped a file into the upload area"

    HandleBrowserAction (Browser.TabSwitch tab) ->
      logShow $ "You just changed to tab: " <> (show tab)

    HandleBrowserAction (Browser.ScrollIsAtBottom) ->
      logShow "Scroll position is now at the bottom of the media container. \
        \ You can use this action to e.g. load more images "

    OpenModal -> H.modify_ _ { isActive = true }

  render :: State -> H.ComponentHTML Action ChildSlots m 
  render state =
    HH.div
      []
      [ HH.button
        [ HE.onClick \_ -> Just OpenModal
        ]
        [ HH.text "Open Modal" ]
      , HH.slot 
        (SProxy :: _ "modal") 
        unit
        Modal.component
        { isActive: state.isActive
        , media: state.media
        , uploadConfig: Just $ UploadConfig
          { maxUploads: 1
          , accept: ["jpg", "png"]
          , acceptError: "File not allowed. Allowed extensions: [jpg, png]"
          , defaultLabel: "Some default label"
          , pendingLabel: "Pending"
          , completedLabel: "Completed"
          , nameLabel: "Name"
          , fileSizeLabel: "Filesize"
          , statusLabel: "Status"
          , uploadThumbnail: Just "/example/static/video.png"
          }
        }
        (Just <<< HandleBrowserAction)
      ]
