module App.Roster.AppService (getTeamRoster, completeDuty, revertDuty, skipMember) where

import App.Roster.DomainService (createDefaultRoster, skipMemberInRoster)
import App.Roster.Repository (findRoster, saveRoster)
import App.Roster.Types as Roster (TeamRoster(..), increaseRosterIndex, decreaseRosterIndex, current)

-- TODO Move to TeamDetails package
import App.TeamDetails.Repository (findTeam, saveTeam)
import App.TeamDetails.Types as Team (TeamDetails(..), Person(..), increaseTimesOnDuty, decreaseTimesOnDuty, newTeam, newPerson)

getTeamRoster :: TeamDetails -> IO TeamRoster
getTeamRoster team = do
                maybeRoster <- findRoster $ Team.teamName team
                case maybeRoster of
                    --If there is no roster, calculate it on the fly
                    Nothing     -> return $ createDefaultRoster team
                    Just roster -> return roster

skipMember :: TeamDetails -> String -> IO ()
skipMember team memberName = do
                roster <- getTeamRoster team
                let newRoster = skipMemberInRoster roster memberName
                saveRoster newRoster


completeDuty :: String -> IO ()
completeDuty tName = do
                maybeTeam <- findTeam tName
                case maybeTeam of
                    Nothing   -> return ()
                    Just team -> calculateNewDuty team

revertDuty :: String -> IO ()
revertDuty tName = do
                maybeTeam <- findTeam tName
                case maybeTeam of
                    Nothing -> return ()
                    Just team -> calculatePreviousDuty team

---- PRIVATE

-- TODO Find another way to do this. Mixing concerns (Saving team and rosters) Also, doesn't look haskelly
calculateNewDuty :: TeamDetails -> IO ()
calculateNewDuty team = do
                 roster <- getTeamRoster team
                 let currentDuty = current roster
                 let newTeam     = increaseTimesOnDuty team currentDuty
                 let newRoster   = increaseRosterIndex roster
                 saveRoster newRoster
                 saveTeam newTeam

calculatePreviousDuty :: TeamDetails -> IO ()
calculatePreviousDuty team = do
                      roster <- getTeamRoster team
                      let newRoster   = decreaseRosterIndex roster
                      let currentDuty = current newRoster
                      let newTeam     = decreaseTimesOnDuty team currentDuty
                      saveRoster newRoster
                      saveTeam newTeam




