module Halogen.Media.Utils where

import Prelude 
import Control.Monad.Except     (runExcept)
import Data.Traversable         (traverse)
import Data.Array               ((..), catMaybes)
import Data.Either              (Either(..))
import Data.Maybe               (Maybe(..))
import Effect                   (Effect, untilE)
import Effect.Class.Console     (logShow)
import Web.File.FileList        as FL
import Web.File.File            as File
import Web.XHR.FormData         as FD

import Halogen.Media.Data.File  (ExtendedFile(..)
                                ,ExtendedFileArray)
import Halogen.Media.FormData   as CFD

-- | Utility function to go from
-- Maybe FL.FileList
-- to Array of File objects
fileListToFiles :: Maybe FL.FileList -> Array File.File
fileListToFiles fl = case fl of
  Just files -> do
    let filesLen = (FL.length files) - 1
    catMaybes $ map (\x -> FL.item x files) (0..filesLen)
  Nothing -> []

-- | Creates a new FormData
--   and adds files to it 
--   with entryName
--   returns the new FormData
filesToFormData :: String 
                -> ExtendedFileArray 
                -> Effect FD.FormData
filesToFormData entryName files = do
  formData <- FD.new
  _ <- traverse (\(ExtendedFile file uuid fileName) ->
    CFD.appendFile (FD.EntryName entryName) file formData)
    files
  pure formData
