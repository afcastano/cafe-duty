module App.Service (Team, getTeam, currentDuty, nextDuty, allDuties) where

import App.RosterGeneration
import App.Repository

getTeam :: String -> Team
getTeam name = myTeam

currentDuty :: (Person,Person)
currentDuty = (generateRoster allPeople) !! 0

nextDuty :: (Person,Person)
nextDuty = (generateRoster allPeople) !! 1

allDuties :: [(Person,Person)]
allDuties = generateRoster allPeople