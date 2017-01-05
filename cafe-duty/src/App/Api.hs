{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.Service

import Web.Scotty
import Control.Monad.IO.Class
import Data.Aeson (ToJSON)

webApi :: ScottyM()
webApi = do
  get "/" $ html "<h1>Robusta Cafe Duty</h1>"
  
  get "/team/:name" $ do
    name <- param "name"
    returnJson $ fetchTeam name

  get "/team/:name/current-duty/" $ do
    name <- param "name"
    returnJson $ currentDuty name

  get "/team/:name/next-duty/" $ do
    name <- param "name"
    returnJson $ nextDuty name  

  get "/team/:name/roster"  $ do
    name <- param "name"
    returnJson $ allDuties name

---- Helper functions
returnJson :: ToJSON a => IO a -> ActionM()
returnJson ioV = do
    val <- liftIO ioV
    json val