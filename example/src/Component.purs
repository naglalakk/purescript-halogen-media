module Example.Component where

import Prelude
import Effect.Class.Console                 (logShow)
import Effect.Class                         (class MonadEffect)
import Effect.Aff.Class                     (class MonadAff)
import Data.Const                           (Const)
import Data.Maybe                           (Maybe(..))
import Halogen                              as H
import Halogen.HTML                         as HH
import Halogen.HTML.Events                  as HE
import Halogen.HTML.Properties              as HP
import Data.Symbol                          (SProxy(..))

import Halogen.Media.Component.Browser      as Browser
import Halogen.Media.Data.Media             (MediaArray
                                            ,Media(..))
import Example.TestData                     (Img, medias)

type State = 
  { media :: MediaArray Img
  }

data Action
  = Initialize
  | HandleBrowserAction (Browser.Output Img)

type Input = Unit

type Query = Const Void

type ChildSlots = (
  mediaBrowser :: H.Slot Query (Browser.Output Img) Unit
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
    { media: []
    }

  handleAction = case _ of
    Initialize -> do
      images <- H.liftEffect $ medias 20
      H.modify_ _ { media = images }

    HandleBrowserAction (Browser.Clicked output) -> do
      logShow "Clicked images:"
      logShow output

    HandleBrowserAction (Browser.InsertedMedia media) -> do
      logShow "You inserted some media collected from the media items you clicked"
      logShow media

    HandleBrowserAction (Browser.Upload files) -> 
      logShow "Hey you just clicked the upload button, good job!"

    HandleBrowserAction (Browser.Dropped files) ->
      logShow "You dropped a file into the upload area"

    HandleBrowserAction (Browser.TabSwitch tab) ->
      logShow $ "You just changed to tab: " <> (show tab)

  render :: State -> H.ComponentHTML Action ChildSlots m 
  render state =
    HH.div
      []
      [ HH.slot 
        (SProxy :: _ "mediaBrowser") 
        unit
        Browser.component
        { media: state.media
        , selectedTab: Nothing }
        (Just <<< HandleBrowserAction)
      ]
