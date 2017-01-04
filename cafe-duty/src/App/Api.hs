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
    returnJson $ getTeam name

  get "/people/current-duty/" $ json currentDuty
  get "/people/next-duty/" $ json nextDuty
  get "/people/roster/" $ json allDuties


returnJson :: ToJSON a => IO (Maybe a) -> ActionM()
returnJson ioV = do
    maybeVal <- liftIO ioV
    json maybeVal