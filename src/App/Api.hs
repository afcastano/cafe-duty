{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.Pages.HomePageService (getHomePageText)
import App.Pages.TeamPageService (getNewTeamPage, getEditTeamPage, getCompleteDutyPage, getTeamListPage)
import App.Pages.ErrorPageService (getErrorPage)

import App.Roster.DomainService (currentDuty, nextDuty)
import App.Roster.AppService (getValidTeam, getTeamRoster, completeDuty, createNewTeam)

import App.TeamDetails.Repository (findTeam, findTeamAndMap, saveMaybeTeam, getTeamNames)
import App.TeamDetails.Types as Team (TeamDetails(..), Person(..), newPerson, addPersonToTeam)

import App.Backup.BackupApi (backupApi)

import Web.Scotty
import Control.Monad.IO.Class
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
    teamName <- param "teamName"
    saveResult <- liftToActionM $ createNewTeam teamName
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
  backupApi




----- Hepler funcitons (Extract to utilities)

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
