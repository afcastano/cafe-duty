module App.Roster.Service (currentDuty, nextDuty, getAllDuties, completeDuty, validateTeam) where

import App.Roster.RosterGeneration (combineElements)
import App.Roster.Types (Team(..), Person(..), increaseRosterIndex, increaseTimesOnDuty)
import App.Helper.Lists (rotate, transformElem)


currentDuty :: Team -> [Person]
currentDuty team = getFst $ getAllDuties team

nextDuty :: Team -> [Person]
nextDuty team = getSnd $ getAllDuties team
          
getAllDuties :: Team -> [[Person]]
getAllDuties team =  generateCurrentRoster team

completeDuty :: Team -> Team
completeDuty team = let updatedTeam = updateTimesOnDuty team
                    in
                    increaseRosterIndex updatedTeam

validateTeam :: Team -> Either String Team
validateTeam team 
              | (length $ members team) < 2 = Left "A team should have at least two members!"
              | otherwise                   = Right team




---------- Helper functions ----

updateTimesOnDuty :: Team -> Team
updateTimesOnDuty team = let onDuty           = currentDuty team
                             membersPartial   = updateTimesOnDutyForPerson (onDuty !! 0) (members team)
                             updatedMembers   = updateTimesOnDutyForPerson (onDuty !! 1) membersPartial
                         in
                         team {members = updatedMembers}

updateTimesOnDutyForPerson :: Person -> [Person] -> [Person]
updateTimesOnDutyForPerson p1 list = transformElem increaseTimesOnDuty p1 list

generateCurrentRoster :: Team -> [[Person]]
generateCurrentRoster team = 
                          let roster  = combineElements $ members team
                          in rotate (rosterIndex team) roster

getIndexSafe :: Int -> [[a]] -> [a]
getIndexSafe _ []        = []
getIndexSafe index list  = (cycle list) !! index

getFst :: [[a]] -> [a]
getFst = getIndexSafe 0

getSnd :: [[a]] -> [a]
getSnd = getIndexSafe 1

