module App.Service (fetchTeam, currentDuty, nextDuty, allDuties) where

import App.RosterGeneration
import App.Repository
import App.Helper.Lists (rotate)

fetchTeam :: String -> IO (Maybe Team) -- Return a result object better
fetchTeam name = findTeam name

currentDuty :: String -> IO [Person]
currentDuty teamName = do
          roster <- allDuties teamName
          return $ roster !! 0 -- make !! 0 safer

nextDuty :: String -> IO [Person]
nextDuty teamName = do
          roster <- allDuties teamName
          return $ roster !! 1 -- make !! 1 safer
          
allDuties :: String -> IO [[Person]]
allDuties teamName = do
          maybeTeam <- findTeam teamName
          return $ generateRosterFrom maybeTeam


---------- Helper functions----
generateRosterFrom :: Maybe Team -> [[Person]]
generateRosterFrom Nothing = [] 
generateRosterFrom (Just team) = 
                                let roster  = generateRoster $ members team
                                in rotate (rosterIndex team) roster