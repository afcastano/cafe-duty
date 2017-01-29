{-# LANGUAGE DeriveDataTypeable #-}
module App.TeamPageService (getNewTeamPage, getEditTeamPage, getCompleteDutyPage, getTeamListPage) where

import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy

import App.Roster.Types(TeamDetails(..), Person(..))

---- UI specific stuff.
--- new team
getNewTeamPage :: IO Text
getNewTeamPage = do
              let context "name" = MuVariable $ "Create" -- DUH????
              useTemplate "templates/new_team.html" context

getCompleteDutyPage :: IO Text
getCompleteDutyPage = do
                  let context "name" = MuVariable $ "Create" -- DUH????
                  useTemplate "templates/complete_duty.html" context

getEditTeamPage :: Maybe TeamDetails -> IO Text
getEditTeamPage Nothing     = populateEditPage $ emptyDto "Not found"
getEditTeamPage (Just team) = populateEditPage $ getDtoFromTeam team

getTeamListPage :: [String] -> IO Text
getTeamListPage names = populateTeamListPage names


--- Helpers
data EditTeamDto = EditTeamDto {
     tName    ::    String
    ,teamMembers :: [String]
} 

emptyDto :: String -> EditTeamDto 
emptyDto tName = EditTeamDto tName []

getDtoFromTeam :: TeamDetails -> EditTeamDto
getDtoFromTeam team = EditTeamDto (teamName team) (Prelude.map name $ members team)

populateEditPage :: EditTeamDto -> IO Text
populateEditPage dto = do
                let context "name"        = MuVariable $ tName dto
                    context "people"      = MuList $ Prelude.map (mkStrContext . mkListContext) (teamMembers dto)
                            where
                            mkListContext p = \"pName"  -> MuVariable $ p
                useTemplate "templates/edit_team.html" context

populateTeamListPage :: [String] -> IO Text
populateTeamListPage names = do
                let context "teams"      = MuList $ Prelude.map (mkStrContext . mkListContext) (names)
                            where
                            mkListContext teamName = \"tName"  -> MuVariable $ teamName
                useTemplate "templates/view_teams.html" context                



-- Consider extracting to helper
useTemplate :: String -> (String -> MuType IO) -> IO Text
useTemplate templateName context = hastacheFile defaultConfig templateName (mkStrContext context)
