{-# LANGUAGE DeriveDataTypeable #-}
module App.Pages.HomePageService (getHomePageText) where

import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy

import App.TeamDetails.Types as Team(TeamDetails(..), Person(..))

import App.Roster.DomainService(currentDuty, nextDuty)
import App.Roster.Types (TeamRoster(..), current, next)

---- UI specific stuff.
--- home page
getHomePageText :: TeamDetails -> TeamRoster -> IO Text
getHomePageText team roster = populateHomePage $ getHomePageDtoFromTeam team roster


--- Helpers
data HomePageDto = HomePageDto {
     tName    ::    String
    ,thisDuty ::    [String]
    ,nxtDuty ::     [String]
    ,teamMembers :: [Person]
}

emptyDto :: String -> HomePageDto 
emptyDto tName = HomePageDto tName [] [] []
      

getHomePageDtoFromTeam :: TeamDetails -> TeamRoster -> HomePageDto
getHomePageDtoFromTeam team roster = let thisDuty    = pairToList $ current roster
                                         nxtDuty     = pairToList $ next roster
                                         teamMembers = members team
                              in HomePageDto (Team.teamName team) thisDuty nxtDuty teamMembers

populateHomePage :: HomePageDto -> IO Text
populateHomePage dto = do
                  let context "name"        = MuVariable $ tName dto
                      context "thisDuty.p1" = MuVariable $ (thisDuty dto) !! 0
                      context "thisDuty.p2" = MuVariable $ (thisDuty dto) !! 1
                      context "nxtDuty.p1"  = MuVariable $ (nxtDuty dto) !! 0
                      context "nxtDuty.p2"  = MuVariable $ (nxtDuty dto) !! 1
                      context "teamId"      = MuVariable $ tName dto
                      context "people"      = MuList $ Prelude.map (mkStrContext . mkListContext) (teamMembers dto)
                            where
                            mkListContext p = \val -> case val of
                                                        "pName"  -> MuVariable $ name p
                                                        "pTimes" -> MuVariable $ timesOnDuty p
                  useTemplate "templates/index.html" context

useTemplate :: String -> (String -> MuType IO) -> IO Text
useTemplate templateName context = hastacheFile defaultConfig templateName (mkStrContext context)

-- TODO Remove. The model should be a pair.
pairToList :: (a,a) -> [a]
pairToList (fst, snd) = [fst, snd]
