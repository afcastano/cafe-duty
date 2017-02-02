{-# LANGUAGE OverloadedStrings #-}
module App.Backup.BackupApi (backupApi) where


import App.Pages.BackupPageService (getBackupPage)

import App.Roster.Repository (findRoster, saveRoster)
import App.Roster.Types as Roster (TeamRoster (..))

import App.TeamDetails.Repository (findTeam, saveMaybeTeam)
import App.TeamDetails.Types as Team (TeamDetails(..))

import Web.Scotty
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, encode, decode)
import Data.Text.Lazy

backupApi :: ScottyM()
backupApi = do

-- backup api
-- Team
  get "/web/backup/team" $ do
    returnHtml $ getBackupPage "team"

  post "/backup/team/restore" $ do
      teamBackup <- param "backupData"
      let teamDetails = (decode teamBackup) :: Maybe TeamDetails
      liftToActionM $ saveMaybeTeam teamDetails
      case teamDetails of
          Nothing   -> text "Invalid backup data!"
          Just team -> redirect $ pack $ "/web/edit/team/" ++ (Team.teamName team)

  get "/api/team/:name" $ do
      name <- param "name"
      returnJson $ findTeam name

-- Roster
  get "/web/backup/roster" $ do
    returnHtml $ getBackupPage "roster"

  post "/backup/roster/restore" $ do
      rosterBackup <- param "backupData"
      let rosterMaybe = (decode rosterBackup) :: Maybe TeamRoster
      case rosterMaybe of
          Nothing     -> text "Invalid backup data!"
          Just roster -> do
                    liftToActionM $ saveRoster roster
                    redirect $ pack $ "/web/team/" ++ (Roster.teamName roster)

  get "/api/roster/:name" $ do
      name <- param "name"
      returnJson $ findRoster name




----- Hepler funcitons

liftToActionM :: IO a -> ActionM a
liftToActionM io = liftAndCatchIO io

returnHtml :: IO Text -> ActionM()
returnHtml ioV = do
    val <- liftAndCatchIO ioV
    html val


returnJson :: ToJSON a => IO a -> ActionM()
returnJson ioA = do
    obj <- liftAndCatchIO ioA
    json obj
