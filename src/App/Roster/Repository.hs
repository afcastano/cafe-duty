{-# LANGUAGE DeriveDataTypeable #-}
module App.Roster.Repository (findRoster, saveRoster, deleteRoster) where

import App.Roster.Types (TeamRoster(..))
import App.Helper.FileDB(findEntity, saveEntity, deleteEntity)

import Control.Exception
import Data.Typeable

findRoster :: String -> IO (Maybe TeamRoster)
findRoster name = findEntity "TeamRoster" name

saveRoster :: TeamRoster -> IO ()
saveRoster roster = saveEntity "TeamRoster" (teamName roster) roster

deleteRoster :: String -> IO ()
deleteRoster teamName = deleteEntity "TeamRoster" teamName