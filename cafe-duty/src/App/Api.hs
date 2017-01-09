{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.HomePageService (getHomePageText)

import App.Roster.Service
import App.Roster.Repository (findTeam,saveTeam, saveMaybe)
import App.Roster.Types(Team(..), Person(..))

import Data.Monoid ((<>))
import Web.Scotty
import Control.Monad.IO.Class
import Data.Aeson (ToJSON)
import Data.Text.Lazy

webApi :: ScottyM()
webApi = do
  get "/" $ html "<h1>Robusta Cafe Duty</h1>"
  
  get "/team/:name" $ do
    name <- param "name"
    returnJson $ findTeam name

  post "/team/:name/complete-duty" $ do
    name <- param "name"
    liftToActionM $ saveMaybe =<< findTeamAndMap completeDuty name
    redirect $ pack $ "/web/team/" ++ name

  get "/team/:name/current-duty/" $ do
    name <- param "name"
    returnJson $ findTeamAndMap currentDuty name

  get "/team/:name/next-duty/" $ do
    name <- param "name"
    returnJson $ findTeamAndMap nextDuty name

  get "/team/:name/roster"  $ do
    name <- param "name"
    returnJson $ findTeamAndMap getAllDuties name

  get "/web/team/:name" $ do
    tName <- param "name"
    returnHtml $ getHomePageText =<< findTeam tName

----- Hepler funcitons        

findTeamAndMap :: (Team -> a) -> String -> IO (Maybe a)
findTeamAndMap mapper teamName = do
                            maybeTeam <- findTeam teamName
                            return $ mapper <$> maybeTeam

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