module Halogen.Media.Component.CSS.Modal where

import Prelude
import Color                                as Color
import CSS                                  as CSS
import CSS                                  ((&),(?))
import CSS.Common                           (center)
import Halogen.HTML.CSS                     as HCSS

import Halogen.Media.Component.CSS.Utils    (padding
                                            ,margin)

stylesheet = HCSS.stylesheet do
  modalContainer
  modalHeader
  media

modalContainer :: CSS.CSS
modalContainer = do
  CSS.fromString ".modal-container" ? do
    CSS.position CSS.relative
    CSS.backgroundColor CSS.white
    margin 25.0
    CSS.zIndex 99999

modalLayer :: Boolean -> CSS.CSS
modalLayer active = do
  CSS.position CSS.fixed
  CSS.display CSS.flex
  CSS.justifyContent $ CSS.JustifyContentValue center
  CSS.alignItems $ CSS.AlignItemsValue center
  CSS.top $ CSS.px 0.0
  CSS.left $ CSS.px 0.0
  CSS.zIndex 99998
  CSS.width $ CSS.pct 100.0
  CSS.height $ CSS.pct 100.0
  CSS.backgroundColor CSS.black

  case active of
    true  -> CSS.display CSS.block
    false -> CSS.display CSS.displayNone

modalHeader :: CSS.CSS
modalHeader = do
  CSS.fromString ".modal-header" ? do
    CSS.position CSS.absolute
    CSS.right $ CSS.px 0.0
    padding 25.0

-- | Todo: This actually does not belong here
--   Move to Browser or MediaDisplay
media :: CSS.CSS
media = do
  CSS.fromString ".media" ? do
    padding 12.5

