{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.HomePageService (getHomePageText)
import App.TeamPageService (getNewTeamPage, getEditTeamPage)

import App.Roster.Service (completeDuty, currentDuty, nextDuty, getAllDuties)
import App.Roster.Repository (findTeam, findTeamAndMap, saveMaybeTeam, saveTeam)
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

-- Web pages
  get "/web/team/:name" $ do
    tName <- param "name"
    returnHtml $ getHomePageText =<< findTeam tName

  get "/web/edit/team/" $ do
    returnHtml $ getNewTeamPage

  get "/web/edit/team/:name" $ do
    tName <- param "name"
    returnHtml $ getEditTeamPage =<< findTeam tName

  post "/edit/team/" $ do
    teamName <- param "teamName"
    liftToActionM $ saveTeam $ newTeam teamName
    redirect $ pack $ "/web/edit/team/" ++ teamName

  post "/edit/team/:name/add-member/" $ do
    teamName <- param "name"
    personName <- param "personName"
    let person = newPerson personName
    liftToActionM $ saveMaybeTeam =<< findTeamAndMap (addPersonToTeam person) teamName
    redirect $ pack $ "/web/edit/team/" ++ teamName


  post "/team/:name/complete-duty" $ do
    name <- param "name"
    liftToActionM $ saveMaybeTeam =<< findTeamAndMap completeDuty name
    redirect $ pack $ "/web/team/" ++ name


----- Hepler funcitons        
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