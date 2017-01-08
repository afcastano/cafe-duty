{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.Service
import App.Repository(Team(..), Person(..))

import Web.Scotty
import Control.Monad.IO.Class
import Data.Aeson (ToJSON)
import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy

webApi :: ScottyM()
webApi = do
  get "/" $ html "<h1>Robusta Cafe Duty</h1>"
  
  get "/team/:name" $ do
    name <- param "name"
    returnJson $ fetchTeam name

  get "/team/:name/complete-duty" $ do
    name <- param "name"
    liftIO $ completeDuty name
    returnJson $ currentDuty name

  get "/team/:name/current-duty/" $ do
    name <- param "name"
    returnJson $ currentDuty name

  get "/team/:name/next-duty/" $ do
    name <- param "name"
    returnJson $ nextDuty name  

  get "/team/:name/roster"  $ do
    name <- param "name"
    returnJson $ getAllDuties name

  get "/web/team/:name" $ do
    teamName <- param "name"
    currentDuty <- liftAndCatchIO $ currentDuty teamName
    let context "name"    = MuVariable teamName
        context "person1" = MuVariable $ name (currentDuty !! 0)
        context "person2" = MuVariable $ name (currentDuty !! 1)
    returnHtml $ hastacheFile defaultConfig "templates/index.html" (mkStrContext context)
    
    
---- Helper functions
returnJson :: ToJSON a => IO a -> ActionM()
returnJson ioV = do
    val <- liftAndCatchIO ioV
    json val

returnHtml :: IO Text -> ActionM()
returnHtml ioV = do
    val <- liftAndCatchIO ioV
    html val