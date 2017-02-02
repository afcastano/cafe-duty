{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.Pages.HomePageService (getHomePageText)
import App.Pages.TeamPageService (getNewTeamPage, getEditTeamPage, getCompleteDutyPage, getTeamListPage)
import App.Pages.ErrorPageService (getErrorPage)
import App.Pages.BackupPageService (getBackupPage)

import App.Roster.DomainService (currentDuty, nextDuty)
import App.Roster.Repository (findRoster, saveRoster)
import App.Roster.AppService (getValidTeam, getTeamRoster, completeDuty)
import App.Roster.Types as Roster (TeamRoster (..))

import App.TeamDetails.Repository (getTeam, findTeam, findTeamAndMap, saveMaybeTeam, getTeamNames, saveNewTeam)
import App.TeamDetails.Types as Team (TeamDetails(..), Person(..), newTeam, newPerson, addPersonToTeam)

import Web.Scotty
import Control.Monad.IO.Class
import Data.Aeson (ToJSON, encode, decode)
import Data.Text.Lazy

webApi :: ScottyM()
webApi = do

-- Web pages api
  get "/" $ do
    redirect "/web/team"

  get "/web/team" $ do
    returnHtml $ getTeamListPage =<< getTeamNames

  get "/web/error/:msg" $ do
    errorMsg <- param "msg"
    returnHtml $ getErrorPage errorMsg

  get "/web/team/:name" $ do
    tName      <- param "name"
    eitherTeam <- liftToActionM $ getValidTeam tName
    case eitherTeam of
        Left msg   -> redirectToError msg
        Right team -> getHomePage team

  get "/web/edit/team/" $ do
    returnHtml $ getNewTeamPage

  get "/web/edit/team/:name" $ do
    tName <- param "name"
    returnHtml $ getEditTeamPage =<< findTeam tName

  get "/web/complete-duty" $ do
    returnHtml $ getCompleteDutyPage

-- form actions
  post "/edit/team/" $ do
    teamName   <- param "teamName"
    saveResult <- liftToActionM $ saveNewTeam $ newTeam teamName
    case saveResult of
        Left msg  -> redirectToError msg
        Right _   -> redirect $ pack $ "/web/edit/team/" ++ teamName

  post "/edit/team/:name/add-member/" $ do
    teamName <- param "name"
    personName <- param "personName"
    let person = newPerson personName
    liftToActionM $ saveMaybeTeam =<< findTeamAndMap (addPersonToTeam person) teamName
    redirect $ pack $ "/web/edit/team/" ++ teamName


  post "/complete-duty" $ do
    name <- param "teamName"
    liftToActionM $ completeDuty name
    redirect $ pack $ "/web/team/" ++ name

-- backup api
-- Team
  get "/web/backup/team" $ do
    returnHtml $ getBackupPage "team"

  post "/backup/team/restore" $ do
      teamBackup <- param "backupData"
      let teamDetails = (decode teamBackup) :: Maybe TeamDetails
      liftToActionM $ saveMaybeTeam teamDetails
      case teamDetails of
          Nothing   -> redirectToError "Invalid backup data!"
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
          Nothing     -> redirectToError "Invalid backup data!"
          Just roster -> do
                    liftToActionM $ saveRoster roster
                    redirect $ pack $ "/web/team/" ++ (Roster.teamName roster)

  get "/api/roster/:name" $ do
      name <- param "name"
      returnJson $ findRoster name




----- Hepler funcitons

getHomePage :: TeamDetails -> ActionM()
getHomePage team = do
                teamRoster <- liftToActionM $ getTeamRoster team
                returnHtml $ getHomePageText team teamRoster

redirectToError :: String -> ActionM()
redirectToError msg = redirect $ pack $ "/web/error/" ++ msg

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