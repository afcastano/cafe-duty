module App.Roster.AppService (getValidTeam, getTeamRoster, completeDuty, createNewTeam, findTeamAndAddPerson) where

-- TODO Move to TeamDetails package
import App.Roster.DomainService (validateTeam, createDefaultRoster, validateTeamName, validatePersonName, tryAddPersonToTeam)
import App.Roster.Repository (findRoster, saveRoster)
import App.Roster.Types as Roster (TeamRoster(..), updateToNextDay, current)

import App.TeamDetails.Repository (getTeam, findTeam, saveTeam, saveNewTeam)
import App.TeamDetails.Types as Team (TeamDetails(..), Person(..), increaseTimesOnDuty, newTeam, newPerson)

-- TODO Move to TeamDetails package
getValidTeam :: String -> IO (Either String TeamDetails)
getValidTeam name = do
              eitherTeam <- getTeam name
              return $ validateTeam =<< eitherTeam

createNewTeam :: String -> IO (Either String TeamDetails)
createNewTeam teamName = do
                let eitherNewTeam = newTeam <$> validateTeamName teamName
                case eitherNewTeam of
                    Left msg   -> return $ Left msg
                    Right team -> saveNewTeam team

findTeamAndAddPerson :: String -> String -> IO (Either String TeamDetails)
findTeamAndAddPerson personName teamName = do
                maybeTeam <- findTeam teamName
                case maybeTeam of
                    Nothing -> return $ Left ("Team " ++ teamName ++ "does not exist")
                    Just team -> liftResult saveWithReturn (validateAndAdd personName team)

--TODO review this two functions Unify the save return type.
saveWithReturn :: TeamDetails -> IO TeamDetails
saveWithReturn team = saveTeam team >> return team

validateAndAdd :: String -> TeamDetails -> Either String TeamDetails
validateAndAdd pName team = tryAddPersonToTeam team =<< newPerson <$> validatePersonName pName

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


--TODO Useless function?
liftResult :: (a -> IO b) -> Either String a -> IO (Either String b)
liftResult _ (Left msg) = return $ Left msg
liftResult f (Right a)  = do
                            myB <- f a
                            return $ Right myB





