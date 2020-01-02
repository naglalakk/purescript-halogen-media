module Halogen.Media.Component.CSS.Browser where

import Prelude
import Color                    as Color
import CSS                      as CSS
import CSS                      ((&),(?))
import CSS.Common               (center)
import CSS.TextAlign            (textAlign, TextAlign(..))
import Halogen.HTML.CSS         as HCSS

stylesheet = HCSS.stylesheet do
  mediaBrowser
  tabs
  tab

mediaBrowser :: CSS.CSS 
mediaBrowser = do
  CSS.fromString ".media-browser" ? do
    CSS.height $ CSS.pct 100.0

tabs :: CSS.CSS
tabs = do
  CSS.fromString ".tabs" ? do
    CSS.display CSS.flex
    CSS.padding pd pd pd pd

  where
    pd = CSS.px 25.0

tab :: CSS.CSS
tab = do
  CSS.fromString ".tab" ? do
    CSS.display CSS.flex
    CSS.justifyContent $ CSS.JustifyContentValue center
    CSS.alignItems $ CSS.AlignItemsValue center
    CSS.color $ Color.rgb 0 0 0
    CSS.backgroundColor $ Color.rgb 255 255 255
    CSS.width $ CSS.px 150.0
    CSS.height $ CSS.px 30.0
    CSS.border CSS.solid (CSS.px 0.5) (Color.rgb 0 0 0)
    textAlign $ TextAlign center
