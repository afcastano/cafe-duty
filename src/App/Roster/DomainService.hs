module App.Roster.DomainService (
    currentDuty,
    nextDuty,
    validateTeam,
    createDefaultRoster,
    validateTeamName,
    validatePersonName,
    tryAddPersonToTeam) where

import App.Roster.RosterGeneration (combineElements, generateInitialRoster, generateNextRoster)
import App.Roster.Types (TeamRoster(..))

import App.TeamDetails.Types as Team (TeamDetails(..), Person(..), increaseTimesOnDuty, findPerson, addPersonToTeam)
import App.Helper.Lists (rotate, transformElem)
import App.Helper.Strings (isEmpty)


currentDuty :: TeamDetails -> [Person]
currentDuty team = getFst $ getAllDuties team

nextDuty :: TeamDetails -> [Person]
nextDuty team = getSnd $ getAllDuties team

-- TODO move to TeamDetails module
validateTeam :: TeamDetails -> Either String TeamDetails
validateTeam team 
              | (length $ members team) < 2 = Left "A team should have at least two members!"
              | otherwise                   = Right team

validateTeamName :: String -> Either String String
validateTeamName teamName
              | isEmpty teamName    = Left "Please enter a non-empty name for the team!"
              | otherwise           = Right teamName

validatePersonName :: String -> Either String String
validatePersonName personName
              | isEmpty personName = Left "Please enter a non-empty name for the person!"
              | otherwise          = Right personName

tryAddPersonToTeam :: TeamDetails -> Person -> Either String TeamDetails
tryAddPersonToTeam team person =
                case (findPerson team (name person)) of
                    Nothing -> Right $ addPersonToTeam person team
                    _       -> Left "There is already a member with that name!"

createDefaultRoster :: TeamDetails -> TeamRoster
createDefaultRoster team = let names         = map name $ members team
                               currentRoster = generateInitialRoster names ""
                               nextRoster    = generateNextRoster currentRoster
                           in TeamRoster (Team.teamName team) currentRoster nextRoster 0

---------- Helper functions ----

getAllDuties :: TeamDetails -> [[Person]]
getAllDuties team =  generateCurrentRoster team

generateCurrentRoster :: TeamDetails -> [[Person]]
generateCurrentRoster team = 
                          let roster  = combineElements $ members team
                          in rotate 1 roster

getIndexSafe :: Int -> [[a]] -> [a]
getIndexSafe _ []        = []
getIndexSafe index list  = (cycle list) !! index

getFst :: [[a]] -> [a]
getFst = getIndexSafe 0

getSnd :: [[a]] -> [a]
getSnd = getIndexSafe 1

