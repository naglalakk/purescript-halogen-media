module Halogen.Media.Utils where

import Prelude

import Data.Array (catMaybes, find, last, (..))
import Data.Foldable (and)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), contains, split, toLower)
import Effect (Effect)
import Halogen.Media.Data.File (ExtendedFile(..))
import Halogen.Media.FormData as CFD
import Web.File.File as File
import Web.File.FileList as FL
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

verifyFiles :: Array String  -> Array File.File -> Boolean
verifyFiles [] files = true
verifyFiles accept files = and results
  where
    results = map (\x -> do
      let
        name = File.name x 
        ext  = last $ split (Pattern ".") name
      case ext of
        Just extension -> 
          isJust $ find (\acc -> toLower acc == toLower extension) accept
        Nothing -> false
    ) files

