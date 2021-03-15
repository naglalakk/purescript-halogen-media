module Halogen.Media.Component.HTML.Utils where

import Prelude
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName
