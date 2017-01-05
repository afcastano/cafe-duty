module App.Service (fetchTeam, currentDuty, nextDuty, allDuties) where

import App.RosterGeneration
import App.Repository

fetchTeam :: String -> IO (Maybe Team)
fetchTeam name = findTeam name

currentDuty :: String -> IO (Person,Person)
currentDuty teamName = do
          roster <- allDuties teamName
          return $ roster !! 0

nextDuty :: String -> IO (Person,Person)
nextDuty teamName = do
          roster <- allDuties teamName
          return $ roster !! 1

allDuties :: String -> IO [(Person,Person)]
allDuties teamName = do
          maybeTeam <- findTeam teamName
          return $ generateRosterForTeam maybeTeam


----- Internal helpers :: PURE :D
generateRosterForTeam :: Maybe Team -> [(Person,Person)]
generateRosterForTeam Nothing = []
generateRosterForTeam (Just team) = generateRoster $ members team