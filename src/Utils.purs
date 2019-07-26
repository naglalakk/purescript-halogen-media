module Halogen.Media.Utils where

import Prelude 
import Control.Monad.Except     (runExcept)
import Effect                   (Effect, untilE)
import Effect.Class.Console     (logShow)
import Data.Array               ((..), catMaybes)
import Data.Either              (Either(..))
import Data.Maybe               (Maybe(..))
import Web.File.FileList        as FL
import Web.File.File            as File


-- | Utility function to go from
-- Maybe FL.FileList
-- to Array of File objects
fileListToFiles :: Maybe FL.FileList -> Array File.File
fileListToFiles fl = case fl of
  Just files -> do
    let filesLen = (FL.length files) - 1
    catMaybes $ map (\x -> FL.item x files) (0..filesLen)
  Nothing -> []
