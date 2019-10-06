module Halogen.Media.FormData where

import Prelude
import Effect               (Effect)
import Effect.Uncurried     as Fn
import Web.File.File        as File
import Web.XHR.FormData     as FD

-- void append(USVString name, USVString value);
foreign import _append :: Fn.EffectFn3 FD.EntryName File.File FD.FormData Unit

-- Appends a Web.File to FormData
appendFile :: FD.EntryName -> File.File -> FD.FormData -> Effect Unit
appendFile name value fd = Fn.runEffectFn3 _append name value fd
