module Halogen.Media.Component.CSS.MediaDisplay where

import Prelude
import Color                    as Color
import CSS                      as CSS
import CSS                      ((&),(?))
import CSS.Geometry             (marginBottom)
import CSS.Overflow             (overflowY, scroll)
import CSS.Common               (center)
import Halogen.HTML.CSS         as HCSS
import Halogen                  as H

stylesheet = HCSS.stylesheet do
  media
  mediaItem
  thumbnail
  thumbnailOverlay
  thumbnailRemove 
  thumbnailImage

media :: CSS.CSS
media = do
  CSS.fromString ".media" ? do
    CSS.display CSS.flex
    CSS.flexWrap CSS.wrap 
    CSS.maxHeight $ CSS.px 650.0
    marginBottom $ CSS.px 100.0
    overflowY scroll

mediaItem :: CSS.CSS
mediaItem = do
  CSS.fromString ".media-item" ? do
    CSS.display CSS.flex
    CSS.justifyContent $ CSS.JustifyContentValue center
    CSS.alignItems $ CSS.AlignItemsValue center
    CSS.padding pd pd pd pd
    where
      pd = CSS.px 12.5

thumbnail :: CSS.CSS
thumbnail = do
  CSS.fromString ".thumbnail" ? do
    CSS.maxWidth $ CSS.px 200.0
    CSS.position CSS.relative

thumbnailOverlay :: CSS.CSS
thumbnailOverlay = do
  CSS.fromString ".thumbnail-overlay" ? do
    CSS.display CSS.flex
    CSS.justifyContent $ CSS.JustifyContentValue center
    CSS.alignItems $ CSS.AlignItemsValue center
    CSS.position CSS.absolute
    CSS.opacity 0.0
    CSS.top  $ CSS.px 0.0
    CSS.left $ CSS.px 0.0
    CSS.width $ CSS.pct 100.0
    CSS.height $ CSS.pct 100.0
  CSS.fromString ".thumbnail-overlay" & CSS.hover ? do
    CSS.backgroundColor $ Color.rgba 0 0 0 0.7
    CSS.opacity 1.0

thumbnailRemove :: CSS.CSS
thumbnailRemove = do
  CSS.fromString ".thumbnail-remove" ? do
    CSS.display CSS.flex
    CSS.justifyContent $ CSS.JustifyContentValue center
    CSS.alignItems $ CSS.AlignItemsValue center
    CSS.border CSS.solid (CSS.px 1.0) (Color.rgb 255 255 255)
    CSS.padding pd pd pd pd
    CSS.color $ (Color.rgb 255 255 255)
  
  CSS.fromString ".thumbnail-remove" & CSS.hover ? do
    CSS.color $ Color.rgb 0 0 0
    CSS.backgroundColor $ Color.rgb 255 255 255

  where
      pd = CSS.px 25.0

thumbnailImage :: CSS.CSS
thumbnailImage = do
  CSS.fromString ".thumbnail-image" ? do
    CSS.width $ CSS.pct 100.0

selected :: Boolean -> CSS.CSS
selected sel = case sel of
  true  -> CSS.backgroundColor $ Color.rgb 105 210 231
  false -> CSS.backgroundColor $ Color.rgb 255 255 255
