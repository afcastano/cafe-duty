module App.Roster.AppService (getValidTeam, getTeamRoster, completeDuty) where

-- TODO Move to TeamDetails package
import App.Roster.DomainService (validateTeam, createDefaultRoster)
import App.Roster.Repository (findRoster, saveRoster)
import App.Roster.Types as Roster (TeamRoster(..), updateToNextDay, current)

import App.TeamDetails.Repository (getTeam, findTeam, saveTeam)
import App.TeamDetails.Types as Team (TeamDetails(..), increaseTimesOnDuty)

-- TODO Move to TeamDetails package
getValidTeam :: String -> IO (Either String TeamDetails)
getValidTeam name = do
              eitherTeam <- getTeam name
              return $ validateTeam =<< eitherTeam

getTeamRoster :: TeamDetails -> IO TeamRoster
getTeamRoster team = do
                maybeRoster <- findRoster $ Team.teamName team
                case maybeRoster of
                    Nothing     -> return $ createDefaultRoster team
                    Just roster -> return roster

-- TODO not clear where this one goes
completeDuty :: String -> IO ()
completeDuty tName = do
                maybeTeam <- findTeam tName
                case maybeTeam of
                    Nothing   -> return ()
                    Just team -> processNewDuty team

---- PRIVATE

-- TODO Find another way to do this. Mixing concerns (Saving team and rosters) Also, doesn't look haskelly
processNewDuty :: TeamDetails -> IO ()
processNewDuty team = do
                roster <- getTeamRoster team
                let currentDuty = current roster
                let newTeam     = increaseTimesOnDuty team currentDuty
                let newRoster   = updateToNextDay roster
                saveRoster newRoster
                saveTeam newTeam



