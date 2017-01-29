module App.Roster.DomainService (currentDuty, nextDuty, validateTeam, createDefaultRoster) where

import App.Roster.RosterGeneration (combineElements, generateInitialRoster, generateNextRoster)
import App.Roster.Types (TeamRoster(..))

import App.TeamDetails.Types as Team (TeamDetails(..), Person(..), increaseTimesOnDuty)
import App.Helper.Lists (rotate, transformElem)


currentDuty :: TeamDetails -> [Person]
currentDuty team = getFst $ getAllDuties team

nextDuty :: TeamDetails -> [Person]
nextDuty team = getSnd $ getAllDuties team
          
--completeDuty :: TeamDetails -> TeamDetails
--completeDuty team = updateTimesOnDuty team

-- TODO move to TeamDetails module
validateTeam :: TeamDetails -> Either String TeamDetails
validateTeam team 
              | (length $ members team) < 2 = Left "A team should have at least two members!"
              | otherwise                   = Right team

createDefaultRoster :: TeamDetails -> TeamRoster
createDefaultRoster team = let names         = map name $ members team
                               currentRoster = generateInitialRoster names ""
                               nextRoster    = generateNextRoster currentRoster
                           in TeamRoster (Team.teamName team) currentRoster nextRoster 0

---------- Helper functions ----

getAllDuties :: TeamDetails -> [[Person]]
getAllDuties team =  generateCurrentRoster team

--updateTimesOnDuty :: TeamDetails -> TeamDetails
--updateTimesOnDuty team = let onDuty           = currentDuty team
--                             membersPartial   = updateTimesOnDutyForPerson (onDuty !! 0) (members team)
--                             updatedMembers   = updateTimesOnDutyForPerson (onDuty !! 1) membersPartial
--                         in
--                         team {members = updatedMembers}

--updateTimesOnDutyForPerson :: Person -> [Person] -> [Person]
--updateTimesOnDutyForPerson p1 list = transformElem increaseTimesOnDuty p1 list

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

