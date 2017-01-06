module App.Service (fetchTeam, currentDuty, nextDuty, allDuties) where

import App.RosterGeneration (generateRoster)
import App.Repository (Team(..), Person, findTeam)
import App.Helper.Lists (rotate)

fetchTeam :: String -> IO (Maybe Team) -- Return a result object better
fetchTeam name = findTeam name

currentDuty :: String -> IO [Person]
currentDuty teamName = do
          roster <- allDuties teamName
          return $ roster `getSafe` 0 -- make !! 0 safer

nextDuty :: String -> IO [Person]
nextDuty teamName = do
          roster <- allDuties teamName
          return $ roster `getSafe` 1 -- make !! 1 safer
          
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

getSafe :: [[a]] -> Int -> [a]
getSafe [] _        = []
getSafe list index  = (cycle list) !! index

