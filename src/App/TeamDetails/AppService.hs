module App.TeamDetails.AppService (getValidTeam, createNewTeam, findTeamAndAddPerson) where

-- TODO Move to TeamDetails package
import App.Roster.DomainService (validateTeam, validateTeamName, validatePersonName, tryAddPersonToTeam)

import App.TeamDetails.Repository (getTeam, findTeam, saveTeam, saveNewTeam)
import App.TeamDetails.Types as Team (TeamDetails(..), Person(..), newTeam, newPerson)

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
                    Just team -> liftResult saveAndReturnSaved (addNewPersonToTeam personName team)

-- Private
---------------

--TODO review this two functions Unify the save return type.
saveAndReturnSaved :: TeamDetails -> IO TeamDetails
saveAndReturnSaved team = saveTeam team >> return team


addNewPersonToTeam :: String -> TeamDetails -> Either String TeamDetails
addNewPersonToTeam pName team = tryAddPersonToTeam team =<< newPerson <$> validatePersonName pName

--TODO Useless function?
liftResult :: (a -> IO b) -> Either String a -> IO (Either String b)
liftResult _ (Left msg) = return $ Left msg
liftResult f (Right a)  = do
                            myB <- f a
                            return $ Right myB





