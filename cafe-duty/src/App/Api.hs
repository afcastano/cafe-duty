{-# LANGUAGE OverloadedStrings #-}
module App.Api (webApi) where

import App.Roster.Service
import App.Roster.Repository (findTeam,saveTeam)
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
    returnJson $ findTeam name

  get "/team/:name/complete-duty" $ do
    name <- param "name"
    maybeTeam <- liftIO $ findTeam name
    case maybeTeam of
      Nothing -> html "Not found"
      Just team -> do 
        let newTeam = completeDuty team
        liftIO $ saveTeam newTeam
        returnJson $ return $ currentDuty newTeam
    
  get "/team/:name/current-duty/" $ do
    name <- param "name"
    returnJson $ fmap (fmap currentDuty) (findTeam name)

  get "/team/:name/next-duty/" $ do
    name <- param "name"
    returnJson $ fmap (fmap nextDuty) (findTeam name)

  get "/team/:name/roster"  $ do
    name <- param "name"
    returnJson $ fmap (fmap getAllDuties) (findTeam name)

  get "/web/team/:name" $ do
    tName <- param "name"
    homePageDto <- liftToActionM $ getHomePageDto tName
    returnHtml $ populateHomePage homePageDto

    
---- UI specific stuff.
--- home page
data HomePageDto = HomePageDto {
     tName    ::    String
    ,thisDuty ::    [String]
    ,nxtDuty ::     [String]
    ,teamMembers :: [Person]
}

emptyTeam :: String -> HomePageDto 
emptyTeam tName = HomePageDto tName [] [] []

getHomePageDto :: String -> IO HomePageDto
getHomePageDto name = do
    maybeTeam <- findTeam name
    case maybeTeam of
      Nothing -> return $ emptyTeam name
      Just team -> return $ getHomePageDtoFromTeam team

getHomePageDtoFromTeam :: Team -> HomePageDto
getHomePageDtoFromTeam team = let thisDuty    = name <$> currentDuty team
                                  nxtDuty     = name <$> nextDuty team
                                  teamMembers = members team
                              in HomePageDto (teamName team) thisDuty nxtDuty teamMembers

populateHomePage :: HomePageDto -> IO Text
populateHomePage dto = do
                  let context "name"    = MuVariable $ tName dto
                      context "person1" = MuVariable $ (thisDuty dto) !! 0
                      context "person2" = MuVariable $ (thisDuty dto) !! 1
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