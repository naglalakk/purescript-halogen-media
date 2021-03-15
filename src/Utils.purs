module Halogen.Media.Utils where

import Prelude
import Data.Array ((..), catMaybes)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Media.Data.File
  ( ExtendedFile(..)
  )
import Halogen.Media.FormData as CFD
import Web.File.FileList as FL
import Web.File.File as File
import Web.XHR.FormData as FD

-- | Utility function to go from
-- Maybe FL.FileList
-- to Array of File objects
fileListToFiles :: Maybe FL.FileList -> Array File.File
fileListToFiles fl = case fl of
  Just files -> do
    let
      filesLen = (FL.length files) - 1
    catMaybes $ map (\x -> FL.item x files) (0 .. filesLen)
  Nothing -> []

-- Convert a single ExtendedFile to FormData
fileToFormData ::
  String ->
  ExtendedFile ->
  Effect FD.FormData
fileToFormData entryName (ExtendedFile file uuid fileName) = do
  formData <- FD.new
  CFD.appendFile (FD.EntryName entryName) file formData
  pure formData
