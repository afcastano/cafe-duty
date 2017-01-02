{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.Service

import Web.Scotty

webApi :: ScottyM()
webApi = do
  get "/" $ html "<h1>Robusta Cafe Duty</h1>"
  get "/people/" $ json allPeople
  get "/people/current-duty/" $ json currentDuty
  get "/people/next-duty/" $ json nextDuty
  get "/people/roster/" $ json generateRoster
