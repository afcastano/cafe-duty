{-# LANGUAGE DeriveDataTypeable #-}
module App.TeamPageService (getNewTeamPage, getEditTeamPage, getCompleteDutyPage) where

import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy

import App.Roster.Types(Team(..), Person(..))

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

getEditTeamPage :: Maybe Team -> IO Text
getEditTeamPage Nothing     = populatePage $ emptyDto "Not found"
getEditTeamPage (Just team) = populatePage $ getDtoFromTeam team


--- Helpers
data EditTeamDto = EditTeamDto {
     tName    ::    String
    ,teamMembers :: [String]
} 

emptyDto :: String -> EditTeamDto 
emptyDto tName = EditTeamDto tName []

getDtoFromTeam :: Team -> EditTeamDto
getDtoFromTeam team = EditTeamDto (teamName team) (Prelude.map name $ members team)

populatePage :: EditTeamDto -> IO Text
populatePage dto = do
                let context "name"        = MuVariable $ tName dto
                    context "people"      = MuList $ Prelude.map (mkStrContext . mkListContext) (teamMembers dto)
                            where
                            mkListContext p = \"pName"  -> MuVariable $ p
                useTemplate "templates/edit_team.html" context



-- Consider extracting to helper
useTemplate :: String -> (String -> MuType IO) -> IO Text
useTemplate templateName context = hastacheFile defaultConfig templateName (mkStrContext context)
