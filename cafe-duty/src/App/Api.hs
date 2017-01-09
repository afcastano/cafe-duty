{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.Roster.Service
import App.Roster.Types(Team(..), Person(..))

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
    tName <- param "name"
    duty     <- liftToActionM $ currentDuty tName
    returnHtml $ populateHomePage duty tName

    
---- Templating - Extract to other file
populateHomePage :: [Person] -> String -> IO Text
populateHomePage duty tName = do
                  let context "name"    = MuVariable tName
                      context "person1" = MuVariable $ name (duty !! 0)
                      context "person2" = MuVariable $ name (duty !! 1)
                  useTemplate "templates/index.html" context

----- Hepler funcitons                  

returnJson :: ToJSON a => IO a -> ActionM()
returnJson ioV = do
    val <- liftAndCatchIO ioV
    json val

useTemplate :: String -> (String -> MuType IO) -> IO Text
useTemplate templateName context = hastacheFile defaultConfig templateName (mkStrContext context)

liftToActionM :: IO a -> ActionM a
liftToActionM io = liftAndCatchIO io

returnHtml :: IO Text -> ActionM()
returnHtml ioV = do
    val <- liftAndCatchIO ioV
    html val