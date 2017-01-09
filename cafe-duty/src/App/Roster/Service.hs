module App.Roster.Service (currentDuty, nextDuty, getAllDuties, completeDuty) where

import App.Roster.RosterGeneration (combineElements)
import App.Roster.Repository (findTeam, saveTeam)
import App.Roster.Types (Team(..), Person(..))
import App.Helper.Lists (rotate)


currentDuty :: Team -> [Person]
currentDuty team = getFst $ getAllDuties team

nextDuty :: Team -> [Person]
nextDuty team = getSnd $ getAllDuties team
          
getAllDuties :: Team -> [[Person]]
getAllDuties team =  generateCurrentRoster team

completeDuty :: Team -> Team
completeDuty team = team {rosterIndex = (rosterIndex team) + 1}



---------- Helper functions ----
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

