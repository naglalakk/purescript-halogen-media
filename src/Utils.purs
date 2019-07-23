module Halogen.Media.Utils where

import Prelude 
import Data.Array           ((..), catMaybes)
import Data.Maybe           (Maybe(..))
import Web.File.FileList    as FL
import Web.File.File        as File

-- Filelists are not so
-- convenient to work with
-- so here we are changing 
-- from type FileList -> Array File
fileListToFiles :: Maybe FL.FileList -> Array File.File
fileListToFiles fl = case fl of
  Just files -> do
    let filesLen = (FL.length files) - 1
    catMaybes $ map (\x -> FL.item x files) (0..filesLen)
  Nothing -> []
