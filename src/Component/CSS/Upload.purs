module Halogen.Media.Component.CSS.Upload where

import Prelude
import Color                                as Color
import CSS                                  as CSS
import CSS                                  ((&),(?))
import CSS.Flexbox                          (flex)
import CSS.Common                           (center, auto)
import CSS.TextAlign                        (textAlign
                                            ,TextAlign(..))
import CSS.Overflow                         (overflow
                                            ,overflowY, scroll)
import CSS.ListStyle.Type                   as LT
import Halogen.HTML.CSS                     as HCSS
import Halogen.Media.Component.CSS.Utils    (padding, margin)

stylesheet = HCSS.stylesheet do
  uploadContainer
  dropbox
  uploads 
  uploadFile
  uploadInfo
  uploadThumbnail

uploadContainer :: CSS.CSS
uploadContainer = do
  CSS.fromString ".upload-container" ? do
    CSS.position CSS.relative
    CSS.marginTop $ CSS.px 25.0
    CSS.display CSS.flex

dropbox :: CSS.CSS
dropbox = do
  CSS.fromString ".dropbox" ? do
    CSS.display CSS.flex
    flex 1 1 auto
    CSS.justifyContent $ CSS.JustifyContentValue center
    CSS.alignItems $ CSS.AlignItemsValue center
    CSS.position CSS.relative
    CSS.border CSS.solid (CSS.px 0.5) (Color.rgb 0 0 0)
    CSS.width $ CSS.pct 100.0
    CSS.minHeight $ CSS.px 300.0
    CSS.marginBottom $ CSS.px 25.0
    margin 25.0
    overflowY scroll
    

uploads :: CSS.CSS
uploads = do
  CSS.fromString ".uploads" ? do
    CSS.display CSS.flex
    CSS.flexWrap CSS.wrap 
    CSS.position CSS.absolute
    CSS.top $ CSS.px 0.0
    CSS.left $ CSS.px 0.0

uploadFile :: CSS.CSS
uploadFile = do
  CSS.fromString ".upload-file" ? do
    CSS.margin md md md md
    CSS.width $ CSS.px 200.0

  where
    md = CSS.px 25.0

uploadInfo :: CSS.CSS
uploadInfo = do
  CSS.fromString ".upload-info" ? do
    CSS.maxWidth $ CSS.pct 100.0
    CSS.margin px px px px
    CSS.padding px px px px
    LT.listStyleType LT.None

  where
    px = CSS.px 0.0


uploadTitle :: CSS.CSS
uploadTitle = do
  CSS.fromString ".upload-title" ? do
    overflow scroll

uploadThumbnail :: CSS.CSS
uploadThumbnail = do
  CSS.fromString ".upload-thumbnail" ? do
    CSS.display CSS.flex
    CSS.justifyContent $ CSS.JustifyContentValue center
    CSS.alignItems $ CSS.AlignItemsValue center
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.px 200.0
    CSS.backgroundRepeat CSS.noRepeat
    CSS.backgroundSize CSS.contain
    CSS.backgroundPosition $ CSS.placed CSS.sideCenter CSS.sideCenter
    CSS.marginBottom $ CSS.px 12.5

  CSS.fromString ".upload-thumbnail > img" ? do
    CSS.width $ CSS.pct 100.0
