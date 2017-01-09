module App.HomePageService (getHomePageText) where

import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy

import App.Roster.Types(Team(..), Person(..))
import App.Roster.Service(currentDuty, nextDuty)

---- UI specific stuff.
--- home page
getHomePageText :: Maybe Team -> IO Text
getHomePageText Nothing     = populateHomePage $ emptyDto "Not found"
getHomePageText (Just team) = populateHomePage $ getHomePageDtoFromTeam team


--- Helpers
data HomePageDto = HomePageDto {
     tName    ::    String
    ,thisDuty ::    [String]
    ,nxtDuty ::     [String]
    ,teamMembers :: [Person]
}

emptyDto :: String -> HomePageDto 
emptyDto tName = HomePageDto tName [] [] []
      

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

useTemplate :: String -> (String -> MuType IO) -> IO Text
useTemplate templateName context = hastacheFile defaultConfig templateName (mkStrContext context)
