module Example.Main where

import Prelude

import Effect                   (Effect)
import Halogen                  as H
import Halogen.Aff              as HA
import Halogen.HTML             as HH
import Halogen.VDom.Driver      (runUI)

import Example.Component        as Example

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  halogenIO <- runUI Example.component unit body
  pure unit
