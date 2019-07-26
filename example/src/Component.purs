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
import Halogen.Media.Data.Base              (MediaArray)
import Example.TestData                     (medias)

type State = 
  { media :: MediaArray
  }

data Action 
  = HandleBrowserAction Browser.Output

type Input = Unit

type Query = Const Void

type ChildSlots = (
  mediaBrowser :: H.Slot Query Browser.Output Unit
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
    }
  }
  where
  initialState :: State
  initialState = 
    { media: medias 20
    }

  handleAction = case _ of
    HandleBrowserAction act -> 
      logShow act

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
