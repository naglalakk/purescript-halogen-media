module Halogen.Media.Component.CSS.Utils where 

import Prelude
import CSS          as CSS

padding :: Number -> CSS.CSS
padding size = CSS.padding 
               (CSS.px size)
               (CSS.px size)
               (CSS.px size)
               (CSS.px size)

margin :: Number -> CSS.CSS
margin size = CSS.margin
              (CSS.px size)
              (CSS.px size)
              (CSS.px size)
              (CSS.px size)
