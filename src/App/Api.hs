{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.HomePageService (getHomePageText)
import App.TeamPageService (getNewTeamPage, getEditTeamPage, getCompleteDutyPage, getTeamListPage)
import App.ErrorPageService (getErrorPage)


import App.Roster.Service (completeDuty, currentDuty, nextDuty, getAllDuties, validateTeam)
import App.Roster.Repository (getTeam, findTeam, findTeamAndMap, saveMaybeTeam, getTeamsName, saveNewTeam)
import App.Roster.Types(Team(..), Person(..), newTeam, newPerson, addPersonToTeam)


import Web.Scotty
import Control.Monad.IO.Class
import Data.Aeson (ToJSON)
import Data.Text.Lazy

webApi :: ScottyM()
webApi = do
-- Rest Api
  get "/team/:name" $ do
    name <- param "name"
    returnJson $ findTeam name

  get "/team/:name/current-duty/" $ do
    name <- param "name"
    returnJson $ findTeamAndMap currentDuty name

  get "/team/:name/next-duty/" $ do
    name <- param "name"
    returnJson $ findTeamAndMap nextDuty name

  get "/team/:name/roster"  $ do
    name <- param "name"
    returnJson $ findTeamAndMap getAllDuties name

-- Web pages api
  get "/" $ do
    redirect "/web/team"

  get "/web/team" $ do
    returnHtml $ getTeamListPage =<< getTeamsName

  get "/web/error/:msg" $ do
    errorMsg <- param "msg"
    returnHtml $ getErrorPage errorMsg

  get "/web/team/:name" $ do
    tName      <- param "name"
    eitherTeam <- getValidTeam tName
    case eitherTeam of
        Left msg   -> redirect $ pack $ "/web/error/" ++ msg
        Right team -> returnHtml $ getHomePageText team

  get "/web/edit/team/" $ do
    returnHtml $ getNewTeamPage

  get "/web/edit/team/:name" $ do
    tName <- param "name"
    returnHtml $ getEditTeamPage =<< findTeam tName

  get "/web/complete-duty" $ do
    returnHtml $ getCompleteDutyPage

  post "/edit/team/" $ do
    teamName   <- param "teamName"
    saveResult <- liftToActionM $ saveNewTeam $ newTeam teamName
    case saveResult of
        Left msg  -> redirect $ pack $ "/web/error/" ++ msg
        Right _   -> redirect $ pack $ "/web/edit/team/" ++ teamName

  post "/edit/team/:name/add-member/" $ do
    teamName <- param "name"
    personName <- param "personName"
    let person = newPerson personName
    liftToActionM $ saveMaybeTeam =<< findTeamAndMap (addPersonToTeam person) teamName
    redirect $ pack $ "/web/edit/team/" ++ teamName


  post "/complete-duty" $ do
    name <- param "teamName"
    liftToActionM $ saveMaybeTeam =<< findTeamAndMap completeDuty name
    redirect $ pack $ "/web/team/" ++ name


----- Hepler funcitons
-- TODO this is messy. Refactor and extract
getValidTeam :: String -> ActionM (Either String Team)
getValidTeam name = do
              eitherTeam <- liftToActionM $ getTeam name
              liftAndCatchIO $ return $ validateTeam =<< eitherTeam

returnJson :: ToJSON a => IO a -> ActionM()
returnJson ioV = do
    val <- liftAndCatchIO ioV
    json val

liftToActionM :: IO a -> ActionM a
liftToActionM io = liftAndCatchIO io

returnHtml :: IO Text -> ActionM()
returnHtml ioV = do
    val <- liftAndCatchIO ioV
    html val